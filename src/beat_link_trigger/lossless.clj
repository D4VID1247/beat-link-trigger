(ns beat-link-trigger.lossless
  "The single, minimal XDJ-AZ Link window. It shows the scrolling
  waveforms of both decks and the essentials about each of them, in
  the style of the XDJ-AZ screen, and hides every setting. Also makes
  sure the DJ Link traffic only travels over the Ethernet cable."
  (:require [beat-carabiner.core :as beat-carabiner]
            [beat-link-trigger.util :as util]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [seesaw.core :as seesaw]
            [taoensso.timbre :as timbre])
  (:import [java.awt Color Font Graphics2D RenderingHints BasicStroke]
           [java.net Inet4Address NetworkInterface]
           [javax.swing ImageIcon JComponent JFrame JOptionPane Timer]
           [org.deepsymmetry.beatlink Beat BeatFinder BeatListener DeviceFinder DeviceUpdate MixerStatus VirtualCdj]
           [org.deepsymmetry.beatlink.data MetadataFinder SearchableItem TimeFinder TrackMetadata
            WaveformDetail WaveformDetailComponent WaveformFinder WaveformPreviewComponent]))

(def ^:private decks
  "The deck numbers shown in the window."
  [1 2])

(def ^:private ^DeviceFinder device-finder (DeviceFinder/getInstance))
(def ^:private ^VirtualCdj virtual-cdj (VirtualCdj/getInstance))
(def ^:private ^MetadataFinder metadata-finder (MetadataFinder/getInstance))
(def ^:private ^TimeFinder time-finder (TimeFinder/getInstance))
(def ^:private ^WaveformFinder waveform-finder (WaveformFinder/getInstance))

(def ^:private usb-port-name
  "The name macOS gives to the network adapter the XDJ-AZ exposes
  through its USB cable. The real Ethernet adapters have other names."
  "usb 10/100 lan")

;;; Keeping the DJ Link traffic on the Ethernet cable

(defn- mac?
  "Checks whether we are running on macOS."
  []
  (str/includes? (str/lower-case (System/getProperty "os.name" "")) "mac"))

(defn- network-services
  "Asks macOS for its network services. Returns a sequence of maps
  holding the `:service` name, the hardware `:port` name, and the
  `:device` name (such as en5)."
  []
  (try
    (let [{:keys [exit out]} (shell/sh "/usr/sbin/networksetup" "-listnetworkserviceorder")]
      (when (zero? exit)
        (for [[_ service port device]
              (re-seq #"\((?:\d+|\*)\)\s+(.+)\n\(Hardware Port: ([^,\n]+), Device: ([^)\s]*)\)" out)]
          {:service (str/trim service)
           :port    (str/trim port)
           :device  device})))
    (catch Throwable t
      (timbre/warn t "Unable to list the macOS network services.")
      nil)))

(defn- device-active?
  "Checks whether the named network device is up with an IPv4 address."
  [^String device]
  (try
    (when-let [iface (when-not (str/blank? device) (NetworkInterface/getByName device))]
      (and (.isUp iface)
           (boolean (some #(instance? Inet4Address %) (enumeration-seq (.getInetAddresses iface))))))
    (catch Throwable _ false)))

(defn- active-usb-service
  "Returns the network service of the XDJ-AZ USB cable when it is
  currently active, or `nil`."
  []
  (first (filter (fn [{:keys [port device]}]
                   (and (= usb-port-name (str/lower-case port))
                        (device-active? device)))
                 (network-services))))

(defn- confirm
  "Shows a yes/no question, returning truthy if the user agreed."
  [^String message ^String yes ^String no]
  (seesaw/invoke-now
   (let [options (to-array [yes no])]
     (zero? (JOptionPane/showOptionDialog nil message "XDJ-AZ Link"
                                          JOptionPane/YES_NO_OPTION JOptionPane/QUESTION_MESSAGE
                                          nil options (aget options 0))))))

(defn- inform
  "Shows a message the user has to acknowledge."
  [^String message]
  (seesaw/invoke-now
   (JOptionPane/showMessageDialog nil message "XDJ-AZ Link" JOptionPane/WARNING_MESSAGE)))

(defn- turn-off-service
  "Asks macOS to deactivate a network service, which makes it request
  the administrator password. Returns truthy if the command worked."
  [^String service]
  (when (re-matches #"[\p{L}\p{N} /.\-()]+" service)
    (let [script (str "do shell script \"/usr/sbin/networksetup -setnetworkserviceenabled '"
                      service "' off\" with administrator privileges")
          result (shell/sh "/usr/bin/osascript" "-e" script)]
      (when-not (zero? (:exit result))
        (timbre/warn "Unable to turn off network service" service (:err result)))
      (zero? (:exit result)))))

(defn ensure-ethernet-only
  "Called before looking for the XDJ-AZ. When the network adapter of
  its USB cable is active, offers to turn it off, so that DJ Link
  information can only arrive through the Ethernet cable and the USB
  cable only carries audio and MIDI. The change is remembered by
  macOS, so this only happens once."
  []
  (when (mac?)
    (try
      (when-let [{:keys [service device]} (active-usb-service)]
        (timbre/info "The USB network of the XDJ-AZ is active:" service device)
        (when (confirm (str "<html>Le XDJ-AZ est aussi relié au Mac par un réseau qui passe dans le câble USB<br>"
                            "(« " service " », " device ").<br><br>"
                            "Pour que XDJ-AZ Link ne reçoive ses informations que par le câble Ethernet,<br>"
                            "ce réseau USB doit être désactivé. L'audio et le MIDI ne sont pas concernés.<br><br>"
                            "macOS va demander le mot de passe du Mac. C'est à faire une seule fois.")
                       "Désactiver le réseau USB" "Plus tard")
          (let [worked (turn-off-service service)]
            (when worked
              (loop [tries 20]  ; Give macOS up to four seconds to take the adapter down.
                (when (and (pos? tries) (device-active? device))
                  (Thread/sleep 200)
                  (recur (dec tries)))))
            (when (or (not worked) (device-active? device))
              (inform (str "<html>Le réseau USB n'a pas pu être désactivé automatiquement.<br><br>"
                           "Il peut l'être à la main : Réglages Système, Réseau, « " service " »,<br>"
                           "puis « Rendre le service inactif »."))))))
      (catch Throwable t
        (timbre/error t "Problem checking that only Ethernet is used.")))))

;;; Gathering what we show

(def ^:private demo?
  "When the LOSSLESS_LINK_DEMO environment variable is set, the window
  shows made-up deck values, so its layout can be checked without an
  XDJ-AZ on the network."
  (some? (System/getenv "LOSSLESS_LINK_DEMO")))

(defn- demo-state
  "Made-up values for a deck, used to check the layout."
  [n]
  (if (= n 1)
    {:title "Deeper Love" :artist "Adriatique" :played 249094 :remain 136094 :pitch -1.92 :bpm 117.7
     :master? false}
    {:title "Illumination" :artist "DJ Koze" :played 204530 :remain 82686 :pitch 2.04 :bpm 122.5
     :master? true}))

(defn- live-state
  "Collects what the players report about a deck right now."
  [n]
  (when (util/online?)
    (let [n                      (int n)
          played                 (when (.isRunning time-finder) (.getTimeFor time-finder n))
          played                 (when (and played (not (neg? played))) played)
          ^WaveformDetail detail (when (.isRunning waveform-finder) (.getLatestDetailFor waveform-finder n))
          total                  (when detail (.getTotalTime detail))
          ^DeviceUpdate update   (when (.isRunning time-finder) (.getLatestUpdateFor time-finder n))
          ^TrackMetadata md      (when (.isRunning metadata-finder) (.getLatestMetadataFor metadata-finder n))
          ^SearchableItem artist (when md (.getArtist md))]
      {:title   (when md (.getTitle md))
       :artist  (when artist (.-label artist))
       :played  played
       :remain  (when (and played total) (max 0 (- total played)))
       :pitch   (when update (org.deepsymmetry.beatlink.Util/pitchToPercentage (.getPitch update)))
       :bpm     (when (and update (not= 65535 (.getBpm update))) (.getEffectiveTempo update))
       :master? (boolean (when update (.isTempoMaster update)))})))

(defn- deck-state
  "Collects what we show about a deck right now."
  [n]
  (if demo? (demo-state n) (live-state n)))

(defn- players-visible?
  "Checks whether we are online and can currently see the XDJ-AZ."
  []
  (boolean (and (util/online?) (seq (.getCurrentDevices device-finder)))))

(defn- ready?
  "Checks whether we are online with the finders the waveform
  components rely on already running, so that creating those
  components cannot stall the window."
  []
  (and (util/online?) (.isRunning waveform-finder)))

(defn- interface-count
  "Counts the network interfaces through which the XDJ-AZ is seen."
  []
  (try
    (count (.getMatchingInterfaces virtual-cdj))
    (catch Throwable _ 0)))

;;; The About window

(defn- show-about
  "Shows the small About window."
  []
  (seesaw/invoke-later
   (let [version (System/getProperty "jpackage.app-version")
         image   (io/resource "images/AZLink.png")
         icon    (when image
                   (ImageIcon. (.getScaledInstance (.getImage (ImageIcon. image)) 128 128 java.awt.Image/SCALE_SMOOTH)))]
     (JOptionPane/showMessageDialog
      nil
      (str "<html><b style='font-size:15pt'>XDJ-AZ Link</b><br>"
           (when version (str "Version " version "<br>"))
           "<br>Relie le XDJ-AZ à Ableton Link par PRO DJ LINK,<br>"
           "uniquement par le câble Ethernet.<br><br>"
           "<span style='color:gray'>Basé sur Beat Link Trigger, de Deep Symmetry<br>"
           "(logiciel libre, licence Eclipse Public License 2.0).</span>")
      "À propos de XDJ-AZ Link" JOptionPane/PLAIN_MESSAGE icon))))

(defn install-about-handler
  "Makes the About item of the macOS application menu show our own
  About window."
  []
  (try
    (when (java.awt.Desktop/isDesktopSupported)
      (let [desktop (java.awt.Desktop/getDesktop)]
        (when (.isSupported desktop java.awt.Desktop$Action/APP_ABOUT)
          (.setAboutHandler desktop (reify java.awt.desktop.AboutHandler
                                      (handleAbout [_this _event]
                                        (show-about)))))))
    (catch Throwable t
      (timbre/warn t "Unable to install the About handler."))))

;;; Drawing

(def ^:private deck-blue (Color. 30 130 235))
(def ^:private master-orange (Color. 255 140 0))
(def ^:private panel-grey (Color. 38 38 40))
(def ^:private label-grey (Color. 165 165 170))
(def ^:private link-green (Color. 60 210 90))
(def ^:private offline-red (Color. 235 60 50))

(defn- font
  "Builds the font we use at the given style and size."
  ^Font [style size]
  (Font. "Helvetica Neue" (int style) (int size)))

(defn- smooth
  "Turns on antialiasing."
  [^Graphics2D g]
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON))

(defn- text
  "Draws a string with its baseline starting at the given point, and
  returns the x coordinate at which it ends."
  [^Graphics2D g ^Font f ^Color color s x y]
  (let [^String s (str s)]
    (.setFont g f)
    (.setColor g color)
    (.drawString g s (int x) (int y))
    (+ x (.stringWidth (.getFontMetrics g) s))))

(defn- text-width
  "Measures a string in a font."
  [^Graphics2D g ^Font f s]
  (.stringWidth (.getFontMetrics g f) ^String (str s)))

(defn- text-right
  "Draws a string so that it ends at the given x coordinate, and
  returns the x coordinate at which it starts."
  [^Graphics2D g ^Font f ^Color color s right y]
  (let [x (- right (text-width g f s))]
    (text g f color s x y)
    x))

(defn- fill
  "Fills a rectangle."
  [^Graphics2D g ^Color color x y w h]
  (.setColor g color)
  (.fillRect g (int x) (int y) (int w) (int h)))

(defn- time-parts
  "Splits a number of milliseconds into the minutes and seconds text
  and the thousandths text."
  [ms]
  (if ms
    (let [ms (long ms)]
      [(format "%02d:%02d" (quot ms 60000) (mod (quot ms 1000) 60))
       (format ".%03d" (mod ms 1000))])
    ["--:--" ".---"]))

(defonce ^:private paint-problem-logged (atom false))

(defn- safely
  "Runs a painting function, logging the first failure only so a
  problem cannot flood the log thirty times a second."
  [f]
  (try
    (f)
    (catch Throwable t
      (when (compare-and-set! paint-problem-logged false true)
        (timbre/error t "Problem painting the XDJ-AZ Link window.")))))

(defn- paint-deck
  "Paints the information panel of a deck."
  [n ^JComponent c ^Graphics2D g]
  (safely
   (fn []
     (let [w                                                  (.getWidth c)
           h                                                  (.getHeight c)
           {:keys [title artist played remain pitch bpm master?]} (deck-state n)
           box-w                                              140
           box-x                                              (- w box-w 14)
           box-y                                              58
           box-h                                              86
           accent                                             (if master? master-orange Color/WHITE)
           [clock millis]                                     (time-parts (or remain played))]
       (smooth g)
       (fill g Color/BLACK 0 0 w h)

       ;; The title bar, with the blue deck tile.
       (fill g panel-grey 0 0 w 46)
       (fill g deck-blue 0 0 64 46)
       (text g (font Font/PLAIN 11) Color/WHITE "DECK" 17 16)
       (text g (font Font/PLAIN 26) Color/WHITE n 25 41)
       (let [old-clip (.getClip g)]
         (.clipRect g 76 0 (int (- w 86)) 46)
         (let [end (text g (font Font/PLAIN 21) Color/WHITE (or title "—") 78 31)]
           (when artist
             (text g (font Font/PLAIN 15) label-grey artist (+ end 14) 31)))
         (.setClip g old-clip))

       ;; The remaining time.
       (text g (font Font/BOLD 12) label-grey (if remain "REMAIN" "TIME") 16 74)
       (let [end (text g (font Font/PLAIN 56) Color/WHITE clock 14 132)]
         (text g (font Font/PLAIN 24) Color/WHITE millis (+ end 2) 132))

       ;; The tempo change, which ends just before the BPM box.
       (let [right (- box-x 18)]
         (text-right g (font Font/BOLD 12) label-grey "TEMPO" right 74)
         (let [percent-x (text-right g (font Font/PLAIN 16) Color/WHITE "%" right 132)]
           (text-right g (font Font/PLAIN 32) Color/WHITE
                       (if pitch
                         (String/format java.util.Locale/ROOT "%+.2f" (to-array [(double pitch)]))
                         "-.--")
                       (- percent-x 2) 132)))

       ;; The BPM box, orange when this deck is the tempo master.
       (.setColor g accent)
       (.setStroke g (BasicStroke. (float 2.0)))
       (.drawRect g (int box-x) (int box-y) (int box-w) (int box-h))
       (text g (font Font/BOLD 12) accent "BPM" (+ box-x 8) (+ box-y 17))
       (when master?
         (text-right g (font Font/BOLD 12) master-orange "MASTER" (+ box-x box-w -8) (+ box-y 17)))
       (let [bpm-text (if bpm
                        (String/format java.util.Locale/ROOT "%.1f" (to-array [(double bpm)]))
                        "---.-")
             dot      (str/index-of bpm-text ".")
             whole    (subs bpm-text 0 dot)
             tenths   (subs bpm-text dot)
             small    (font Font/PLAIN 28)
             tenths-x (text-right g small accent tenths (+ box-x box-w -10) (+ box-y box-h -14))]
         (text-right g (font Font/PLAIN 48) accent whole tenths-x (+ box-y box-h -14)))))))

(defn- paint-deck-label
  "Paints the label to the left of a deck's scrolling waveform."
  [n ^JComponent c ^Graphics2D g]
  (safely
   (fn []
     (let [w (.getWidth c)
           h (.getHeight c)]
       (smooth g)
       (fill g Color/BLACK 0 0 w h)
       (fill g panel-grey 0 0 (- w 4) 34)
       (fill g deck-blue 0 9 16 16)
       (text g (font Font/BOLD 15) deck-blue (str "DECK " n) 24 23)))))

(defonce ^{:private true
           :doc     "Whether the scrolling waveforms are currently shown."}
  waveforms-shown (atom false))

(defonce ^{:private true
           :doc     "Whether the two player panels are currently shown."}
  players-shown (atom false))

(def ^:private toggle-x
  "Where the control that shows or hides the scrolling waveforms starts
  in the header."
  150)

(def ^:private toggle-end
  "Where that control ends, and the one for the players starts."
  276)

(def ^:private players-toggle-end
  "Where the control that shows or hides the players ends."
  380)

(defn- paint-toggle
  "Paints a control that shows or hides part of the window: a triangle
  pointing down when that part is shown, right when it is hidden,
  followed by a label."
  [^Graphics2D g ^Font f x shown? label]
  (.setColor g label-grey)
  (if shown?
    (.fillPolygon g (int-array [x (+ x 10) (+ x 5)]) (int-array [17 17 25]) 3)
    (.fillPolygon g (int-array [(+ x 1) (+ x 9) (+ x 1)]) (int-array [15 21 27]) 3))
  (text g f label-grey label (+ x 17) 26))

(defn- link-bpm
  "Returns the tempo of the Ableton Link session when we are connected."
  []
  (when (beat-carabiner/active?) (:link-bpm (beat-carabiner/state))))

(defonce ^{:private true
           :doc     "When the mixer section of the XDJ-AZ last sent a beat, and at what tempo."}
  mixer-beat (atom nil))

(def ^:private mixer-beat-listener
  "Notices the beats sent by the mixer section of the XDJ-AZ, which
  follow the tempo its Beat FX detects, rather than those of a deck."
  (reify BeatListener
    (newBeat [_this beat]
      (let [^Beat beat beat]
        (when (> (.getDeviceNumber beat) 6)  ; Decks use the low numbers, the mixer uses 33.
          (reset! mixer-beat {:at  (System/currentTimeMillis)
                              :bpm (.getEffectiveTempo beat)}))))))

(defn- beat-fx-state
  "Reports what the mixer section of the XDJ-AZ tells us about the
  tempo its Beat FX detects: whether the mixer is `:seen?` at all, the
  `:bpm` it reports if any, and whether its `:beats?` are arriving."
  []
  (cond
    demo?
    {:seen? true :bpm 117.3 :beats? true}

    (players-visible?)
    (let [^MixerStatus status (first (filter #(instance? MixerStatus %) (.getLatestStatus virtual-cdj)))
          beat                @mixer-beat
          beats?              (boolean (and beat (< (- (System/currentTimeMillis) (long (:at beat))) 2000)))
          bpm                 (cond
                                (and status (not= 65535 (.getBpm status))) (.getEffectiveTempo status)
                                beats?                                     (:bpm beat))]
      {:seen?  (or (some? status) beats?)
       :bpm    (when bpm (/ (Math/round (* 10.0 (double bpm))) 10.0))
       :beats? beats?})))

(defn- paint-header
  "Paints the bar at the top of the window, with the control that
  shows the scrolling waveforms, and the state of the Ethernet
  connection and of Ableton Link."
  [^JComponent c ^Graphics2D g]
  (safely
   (fn []
     (let [w          (.getWidth c)
           h          (.getHeight c)
           online?    (players-visible?)
           interfaces (if online? (interface-count) 0)
           link-bpm   (link-bpm)
           small      (font Font/BOLD 13)
           status     (fn [^Color color label right]
                        (let [x (text-right g small color label right 26)]
                          (.setColor g color)
                          (.fillOval g (int (- x 16)) 16 10 10)
                          (- x 40)))]
       (smooth g)
       (fill g Color/BLACK 0 0 w h)
       (text g (font Font/BOLD 17) Color/WHITE "XDJ-AZ LINK" 16 27)

       ;; The controls that show or hide the scrolling waveforms and the players.
       (paint-toggle g small toggle-x @waveforms-shown "WAVEFORMS")
       (paint-toggle g small (+ toggle-end 8) @players-shown "PLAYERS")
       (let [right (- w 16)
             right (if link-bpm
                     (status link-green
                             (String/format java.util.Locale/ROOT "ABLETON LINK  %.2f BPM"
                                            (to-array [(double link-bpm)]))
                             right)
                     (status label-grey "ABLETON LINK" right))
             right (cond
                     (not online?)    (status offline-red "ETHERNET" right)
                     (> interfaces 1) (status master-orange "ETHERNET + USB" right)
                     :else            (status link-green "ETHERNET" right))]
         ;; The tempo detected by the Beat FX: green while the mixer also sends its beats.
         (when-let [{:keys [seen? bpm beats?]} (beat-fx-state)]
           (status (if beats? link-green label-grey)
                   (cond
                     (not seen?) "BEAT FX ABSENT"
                     bpm         (String/format java.util.Locale/ROOT "BEAT FX  %.1f BPM" (to-array [(double bpm)]))
                     :else       "BEAT FX  --.-")
                   right)))))))

;;; The window

(defonce ^{:private true
           :doc     "Holds the window once it has been created."}
  window (atom nil))

(defn- waveform-row
  "Builds the row holding a deck's label and its scrolling waveform."
  [n detail]
  (seesaw/border-panel :background Color/BLACK
                       :west (seesaw/canvas :paint (partial paint-deck-label n) :preferred-size [100 :by 120])
                       :center detail))

(defn- create-window
  "Builds the XDJ-AZ Link window. `quit-fn` is called when the user
  closes it. The scrolling waveforms and the players both start
  hidden, leaving only the header. The scrolling waveforms are only
  built and animated while they are shown, because drawing them is
  what keeps the computer busy."
  [quit-fn]
  (let [holders  (mapv (fn [_]  ; Each receives the whole-track waveform of its deck once we are online.
                         (seesaw/border-panel
                          :background Color/BLACK
                          :center (seesaw/canvas :background Color/BLACK :preferred-size [400 :by 60])))
                       decks)
        previews (atom nil)
        header   (seesaw/canvas :paint paint-header :preferred-size [1100 :by 40])
        scrolls  (seesaw/grid-panel :rows (count decks) :columns 1 :vgap 6 :background Color/BLACK
                                    :visible? false)
        details  (atom nil)
        infos    (mapv (fn [n]
                         (seesaw/canvas :paint (partial paint-deck n) :preferred-size [540 :by 156]))
                       decks)
        panels   (seesaw/grid-panel :rows 1 :columns (count decks) :hgap 14 :background Color/BLACK
                                    :items (mapv (fn [info preview]
                                                   (seesaw/border-panel
                                                    :background Color/BLACK
                                                    :center info
                                                    :south preview))
                                                 infos holders))
        content  (seesaw/border-panel :background Color/BLACK :vgap 8 :border 10
                                      :north header :center scrolls :south panels)
        ^JFrame root (seesaw/frame :title "XDJ-AZ Link" :on-close :nothing :content content)
        fit      (fn []  ; Never change the width; take the height the content needs, within the screen.
                   (let [screen (if-let [config (.getGraphicsConfiguration root)]
                                  (.getBounds config)
                                  (java.awt.Rectangle. 0 0 1440 900))
                         width  (min (.getWidth root) (.-width screen))
                         height (min (.-height (.getPreferredSize root)) (.-height screen))]
                     (.setSize root (int width) (int height))
                     ;; Move the window up if unfolding it pushed its bottom off the screen.
                     (let [lowest (- (+ (.-y screen) (.-height screen)) height)]
                       (when (> (.getY root) lowest)
                         (.setLocation root (.getX root) (int (max (.-y screen) lowest)))))
                     (.validate root)))
        show     (fn [shown?]
                   (reset! waveforms-shown shown?)
                   (when (and shown? (nil? @details))
                     (let [ready (ready?)
                           built (mapv (fn [n]
                                         (if ready
                                           (doto (WaveformDetailComponent. (int n))
                                             (.setScale 1))
                                           (seesaw/canvas :background Color/BLACK)))
                                       decks)]
                       (when ready (reset! details built))  ; Offline placeholders get rebuilt once we are online.
                       (seesaw/config! scrolls :items (mapv waveform-row decks built))))
                   (doseq [[n detail] (map vector decks @details)]
                     (when (instance? WaveformDetailComponent detail)
                       (.setMonitoredPlayer ^WaveformDetailComponent detail (int (if shown? n 0)))))
                   (seesaw/config! scrolls :visible? shown?)
                   (fit)
                   (.repaint ^JComponent header))
        players  (fn [shown?]
                   (reset! players-shown shown?)
                   (seesaw/config! panels :visible? shown?)
                   (fit)
                   (.repaint ^JComponent header))
        painted  (atom nil)
        timer    (Timer. 66 (reify java.awt.event.ActionListener
                              (actionPerformed [_this _event]
                                ;; As soon as we are online, bring in the waveforms that had to wait.
                                (when (ready?)
                                  (when (nil? @previews)
                                    (reset! previews (mapv (fn [n] (WaveformPreviewComponent. (int n))) decks))
                                    (doseq [[holder preview] (map vector holders @previews)]
                                      (seesaw/config! holder :center preview))
                                    (fit))
                                  (when (and @waveforms-shown (nil? @details))
                                    (show true)))
                                ;; Only redraw when something we show has changed.
                                (let [now [(mapv deck-state decks) (players-visible?) (interface-count) (link-bpm) (beat-fx-state)]]
                                  (when (not= now @painted)
                                    (reset! painted now)
                                    (.repaint ^JComponent header)
                                    (when @players-shown
                                      (doseq [^JComponent info infos]
                                        (.repaint info))))))))]
    (.addBeatListener (BeatFinder/getInstance) mixer-beat-listener)
    (.setMinimumSize root (java.awt.Dimension. 1000 60))
    (seesaw/config! panels :visible? @players-shown)
    (.pack root)  ; Makes the window measurable; its real size is set just below.
    (.setSize root 1120 (.getHeight root))
    (util/restore-window-position root :lossless nil)
    (fit)
    (seesaw/listen header :mouse-pressed (fn [^java.awt.event.MouseEvent e]
                                           (cond
                                             (< (.getX e) toggle-end)
                                             (show (not @waveforms-shown))

                                             (< (.getX e) players-toggle-end)
                                             (players (not @players-shown)))))
    (seesaw/listen root
                   :window-closing (fn [_] (quit-fn))
                   :component-moved (fn [_] (util/save-window-position root :lossless))
                   :component-resized (fn [_] (util/save-window-position root :lossless)))
    (.start timer)
    root))

(defn show-window
  "Shows the XDJ-AZ Link window, creating it the first time.
  `quit-fn` is called when the user closes the window."
  [quit-fn]
  (seesaw/invoke-now
   (try
     (when-not @window
       (reset! window (create-window quit-fn)))
     (let [^JFrame root @window]
       (seesaw/show! root)
       (.toFront root))
     (catch Throwable t
       (timbre/error t "Problem showing the XDJ-AZ Link window.")))))

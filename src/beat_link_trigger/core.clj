(ns beat-link-trigger.core
  "Top level organization for starting up the interface, logging, and
  managing online presence."
  (:require [beat-link-trigger.help :as help]
            [beat-link-trigger.logs :as logs]
            [beat-link-trigger.lossless :as lossless]
            [beat-link-trigger.menus :as menus]
            [beat-link-trigger.prefs :as prefs]
            [beat-link-trigger.settings :as settings]
            [beat-link-trigger.show :as show]
            [beat-link-trigger.show-util :as su :refer [in-show-ns]]
            [beat-link-trigger.triggers :as triggers]
            [beat-link-trigger.util :as util :refer [in-core-ns in-triggers-ns]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [seesaw.core :as seesaw]
            [taoensso.timbre :as timbre])
  (:import [org.deepsymmetry.beatlink DeviceFinder VirtualCdj]))

(def ^DeviceFinder device-finder
  "A convenient reference to the [Beat Link
  `DeviceFinder`](https://deepsymmetry.org/beatlink/apidocs/org/deepsymmetry/beatlink/DeviceFinder.html)
  singleton."
  (DeviceFinder/getInstance))

(def ^VirtualCdj virtual-cdj
  "A convenient reference to the [Beat Link
  `VirtualCdj`](https://deepsymmetry.org/beatlink/apidocs/org/deepsymmetry/beatlink/VirtualCdj.html)
  singleton."
  (VirtualCdj/getInstance))

(def dynamic-class-loader
  "The class loader that supports dynamic additions, for users to be
  able to pull in new dependencies. This gets initialized in `start`."
  (atom nil))

(defn finish-startup
  "Called when we have successfully gone online, or the user has said
  they want to proceed offline. If present, the `show` and `suppress`
  keys come from our command-line arguments, telling us which shows
  should be opened."
  [{:keys [show suppress]}]
  (seesaw/invoke-now
   (.setContextClassLoader (Thread/currentThread) @dynamic-class-loader)  ; This is getting reset somewhere.
   (when (triggers/start)             ; Set up the Triggers window, and check if it was created for the first time.
     ;; This is inital startup, so see if we are supposed to open any shows, including those the user had open
     ;; during their previous session.
     (when-not suppress
       (show/reopen-previous-shows))
     (show/reopen-specified-shows show))
   (lossless/show-window triggers/quit)))  ; The only window XDJ-AZ Link shows.

(defn try-going-online
  "Search for the XDJ-AZ on the DJ Link network for as long as it takes,
  without presenting any dialog: the main window is shown right away,
  and reports whether the players can be seen. If present, `options`
  holds the command-line options we were given at application startup,
  to control which shows are opened."
  ([]
   (try-going-online nil))
  ([options]
   (let [real-player? (triggers/real-player?)
         last-problem (atom nil)]  ; So we only log a recurring problem once.
     (lossless/show-window triggers/quit)  ; The only window XDJ-AZ Link shows, visible while we search.
     (lossless/ensure-ethernet-only)       ; Keep DJ Link traffic off the USB cable before we start listening.
     (timbre/info "Trying to go online, Use Real Player Number?" real-player?)
     (loop []
       (let [online? (try
                       (.start device-finder)  ; Has no effect if it is already running.
                       (when (seq (.getCurrentDevices device-finder))
                         (.setUseStandardPlayerNumber virtual-cdj real-player?)
                         (if (.start virtual-cdj)
                           (do
                             (timbre/info "Went online, using player number" (.getDeviceNumber virtual-cdj)
                                          "and Opus Quad compatibility mode is"
                                          (.inOpusQuadCompatibilityMode virtual-cdj))
                             (when-let [interfaces (seq (help/list-conflicting-network-interfaces))]
                               (timbre/warn "Found multiple network interfaces on the DJ Link network:" interfaces))
                             true)
                           (throw (Exception. "Unable to create Virtual CDJ"))))
                       (catch Throwable t
                         (when (not= (str t) @last-problem)
                           (reset! last-problem (str t))
                           (timbre/warn t "Unable to go online yet, will keep trying."))
                         false))]
         (when-not online?
           (Thread/sleep 500)
           (recur)))))

   (finish-startup options)))

(defn start
  "Set up logging, set up our user interface look-and-feel, then make
  sure we can start the [[virtual-cdj]]. If all went well, present the
  Triggers interface. Called when jar startup has detected a
  recent-enough Java version to succcessfully load this namespace, and
  that passes the parsed command-line options."
  ([]
   (start {}))
  ([{:keys [offline reset config] :as options}]
   (logs/init-logging)
   (timbre/info "Beat Link Trigger starting.")

   ;; Set up a dynamic class loader so that users can add new jars or Maven dependencies for their
   ;; expression code at runtime.
   (let [cl (clojure.lang.DynamicClassLoader. (clojure.lang.RT/baseLoader))]
     (.bindRoot Compiler/LOADER cl)
     (reset! dynamic-class-loader cl)
     (.setContextClassLoader (Thread/currentThread) cl)

     ;; Set the actual application name for the macOS menu bar.
     ;; Sadly, this no longer works in current macOS versions, so we
     ;; need to continue to rely on the bundle descriptor, and lack
     ;; a valid name when running in development or as a loose jar.
     (System/setProperty "apple.awt.application.name" "XDJ-AZ Link")

     ;; Allow dark title bars. This is supposed to be back-ported to Java 11.0.8,
     ;; but it is not yet working either.
     (System/setProperty "apple.awt.application.appearance" "system")

     ;; Switch to the Swing Event Dispatch Thread to configure the user interface.
     (seesaw/invoke-now
       (seesaw/native!) ; Adopt as native a look-and-feel as possible.
       (System/setProperty "apple.laf.useScreenMenuBar" "false") ; Except put menus in frames.
       (prefs/set-ui-theme)

       ;; Use our dynamic class loader on the Swing thread too.
       (.setContextClassLoader (Thread/currentThread) cl))

     ;; If we are on a Mac, hook up our About and Settings handlers
     ;; where users expect to find them, and add a Quit handler that
     ;; saves the state, and gives users a chance to veto losing
     ;; unsaved editor windows.
     ;; XDJ-AZ Link has no About window and no settings, so it only installs the Quit handler.
     (menus/install-mac-quit-handler)

     ;; If we were given instructions to reset our preferences, try to do so.
     (when-not (str/blank? reset)
       (let [file (io/file reset)]
         (if (= (util/file-type file) :configuration)
           (try
             (prefs/save-to-file file)
             (prefs/put-preferences {})  ; Wipe out the preferences we just backed up.
             (catch Throwable t
               (println "--reset: problem writing existing configuration to file" reset)
               (println "  cause:" t)
               (System/exit 1)))
           (do
             (println "--reset: file" reset "does not have required extension:"
                      (str "." (util/extension-for-file-type :configuration)))
             (System/exit 1)))))

     ;; If we were told to use a specific configuration file, try to do so.
     (when-not (str/blank? config)
       (let [file (io/file config)]
         (if (= (util/file-type file) :configuration)
           (seesaw/invoke-now
             (prefs/load-from-file file))
           (do
             (println "--config: file" config "does not have required extension:"
                      (str "." (util/extension-for-file-type :configuration)))
             (System/exit 1)))))

     (let [preferences (prefs/get-preferences)]

       ;; Restore saved window positions if they exist
       (when-let [saved (:window-positions preferences)]
         (reset! util/window-positions saved))

       ;; Set up to update the user interface theme if we should respect system light/dark settings.
       (.registerListener prefs/theme-detector
                          (reify java.util.function.Consumer
                            (accept [_this dark?]
                              (prefs/set-ui-theme dark?))))

       ;; Honor the user's preferred waveform style
       (let [selected-style (get settings/wave-styles (:waveform-style preferences "RGB"))]
         (if selected-style
           (.setPreferredStyle (org.deepsymmetry.beatlink.data.WaveformFinder/getInstance) selected-style)
           (timbre/error "Unsupported :waveform-style found in preferences:" (:waveform-style preferences)))))

     (if offline
       (finish-startup options)        ; User did not want to go online.
       (try-going-online options)))))  ; Normal startup, try finding a Pioneer DJ Link network.

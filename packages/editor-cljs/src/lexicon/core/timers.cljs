(ns lexicon.core.timers
  "Timer system implementing Emacs timer API.

  Provides run-with-timer, run-with-idle-timer, cancel-timer using
  js/setTimeout and js/setInterval. Tracks timers in an atom mapping
  timer-id to JS handle for cancellation.

  Idle timers track elapsed time since last user input. The last-user-input-time
  atom is updated by the keymap dispatcher on every keystroke."
  (:require [lexicon.core.log :as log]))

;; =============================================================================
;; Timer State
;; =============================================================================

;; Maps timer-id (keyword) -> {:js-handle number :repeat? boolean :type :timer|:idle}
(defonce active-timers (atom {}))

;; Monotonic counter for unique timer IDs
(defonce ^:private timer-counter (atom 0))

;; Tracks the last time user provided input (keystroke, mouse click, etc.)
;; Updated by the command dispatch path.
(defonce last-user-input-time (atom (js/Date.now)))

(defn update-last-input-time!
  "Record that user input just occurred. Called from command dispatch."
  []
  (reset! last-user-input-time (js/Date.now)))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- next-timer-id
  "Generate a unique timer ID keyword."
  []
  (keyword (str "timer-" (swap! timer-counter inc))))

(defn- secs->ms
  "Convert seconds (possibly fractional) to milliseconds."
  [secs]
  (max 0 (int (* (or secs 0) 1000))))

(defn- invoke-timer-fn
  "Safely invoke a timer callback function with args."
  [timer-id f args]
  (try
    (apply f args)
    (catch :default e
      (log/warn (str "Timer " timer-id " callback error: " e)))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn run-with-timer
  "Run FN after SECS seconds. If REPEAT is non-nil (a number), repeat every
   REPEAT seconds after the initial delay.

   Returns a timer-id that can be passed to cancel-timer.

   Usage: (run-with-timer 2 nil (fn [] (println \"fired!\")))
          (run-with-timer 0 5 (fn [] (println \"every 5s\")))"
  [secs repeat f & args]
  (let [timer-id (next-timer-id)
        delay-ms (secs->ms secs)
        args (vec args)]
    (if (and repeat (number? repeat) (pos? repeat))
      ;; Repeating timer: use setTimeout for initial delay, then setInterval
      (let [repeat-ms (secs->ms repeat)
            ;; We use setTimeout first, then switch to setInterval
            interval-handle (atom nil)
            initial-handle
            (js/setTimeout
             (fn []
               (invoke-timer-fn timer-id f args)
               ;; Now start repeating
               (let [h (js/setInterval
                        (fn []
                          (invoke-timer-fn timer-id f args))
                        repeat-ms)]
                 (reset! interval-handle h)
                 ;; Update the stored handle
                 (swap! active-timers update timer-id
                        assoc :js-handle h :phase :interval)))
             delay-ms)]
        (swap! active-timers assoc timer-id
               {:js-handle initial-handle
                :phase :timeout
                :interval-handle interval-handle
                :repeat? true
                :type :timer})
        timer-id)
      ;; One-shot timer
      (let [handle (js/setTimeout
                    (fn []
                      (invoke-timer-fn timer-id f args)
                      ;; Clean up after firing
                      (swap! active-timers dissoc timer-id))
                    delay-ms)]
        (swap! active-timers assoc timer-id
               {:js-handle handle
                :repeat? false
                :type :timer
                :phase :timeout})
        timer-id))))

(defn run-with-idle-timer
  "Run FN after SECS seconds of idle time (no user input).

   If REPEAT is non-nil, re-arm after each firing (will fire again
   after another SECS seconds of idleness).

   Idle time = time since last keystroke/command.

   Returns a timer-id that can be passed to cancel-timer."
  [secs repeat f & args]
  (let [timer-id (next-timer-id)
        idle-ms (secs->ms secs)
        args (vec args)
        ;; Check function: see if we've been idle long enough
        check-fn (fn check []
                   (let [elapsed (- (js/Date.now) @last-user-input-time)]
                     (if (>= elapsed idle-ms)
                       ;; Idle long enough — fire!
                       (do
                         (invoke-timer-fn timer-id f args)
                         (if repeat
                           ;; Re-arm: check again after idle-ms
                           (let [h (js/setTimeout check idle-ms)]
                             (swap! active-timers update timer-id assoc :js-handle h))
                           ;; One-shot: clean up
                           (swap! active-timers dissoc timer-id)))
                       ;; Not idle long enough — reschedule for remaining time
                       (let [remaining (- idle-ms elapsed)
                             h (js/setTimeout check remaining)]
                         (swap! active-timers update timer-id assoc :js-handle h)))))
        ;; Start the initial check
        handle (js/setTimeout check-fn idle-ms)]
    (swap! active-timers assoc timer-id
           {:js-handle handle
            :repeat? (boolean repeat)
            :type :idle
            :phase :timeout})
    timer-id))

(defn cancel-timer
  "Cancel a timer. TIMER-ID is the value returned by run-with-timer
   or run-with-idle-timer. Returns nil."
  [timer-id]
  (when-let [timer-info (get @active-timers timer-id)]
    (let [{:keys [js-handle phase interval-handle]} timer-info]
      ;; Cancel the current JS handle
      (case phase
        :interval (js/clearInterval js-handle)
        :timeout (js/clearTimeout js-handle)
        ;; Default: try both
        (do (js/clearTimeout js-handle)
            (js/clearInterval js-handle)))
      ;; For repeating timers that may have a pending interval handle
      (when interval-handle
        (when-let [ih @interval-handle]
          (js/clearInterval ih)))
      ;; Remove from tracking
      (swap! active-timers dissoc timer-id)))
  nil)

(defn cancel-all-timers!
  "Cancel all active timers. Used for cleanup/testing."
  []
  (doseq [[timer-id _] @active-timers]
    (cancel-timer timer-id))
  (reset! active-timers {}))

(ns lexicon.effects.log
  "Re-frame effect handlers for log system (Issue #73)"
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.log :as log]))

;; Effect handler to attach Messages buffer to log bus
(rf/reg-fx
 :log/attach-messages-buffer
 (fn [_]
   "Attach *Messages* buffer to log bus with history replay.

   This effect should be dispatched once the *Messages* buffer is created.
   It will replay all historical logs and attach the sink for future logs."
   (log/attach-messages-buffer!
    (fn [text]
      ;; Append text to Messages buffer directly via WASM
      (let [db @re-frame.db/app-db
            messages-buffer (get-in db [:buffers 2])
            ^js wasm-instance (:wasm-instance messages-buffer)]
        (when wasm-instance
          (let [buffer-length (.length wasm-instance)]
            ;; Append text to end of buffer (no timestamp - already in log entry)
            (.insert wasm-instance buffer-length text)
            ;; Update cache
            (let [updated-text (.getText wasm-instance)
                  updated-line-count (count (clojure.string/split updated-text #"\n" -1))]
              (rf/dispatch-sync [:buffer/update-cache 2 updated-text updated-line-count])
              (rf/dispatch-sync [:buffer/increment-version 2])))))))))

;; Register this namespace to ensure effect handlers are loaded
(defn register-effects! []
  ;; Effect handlers are registered via reg-fx above
  nil)

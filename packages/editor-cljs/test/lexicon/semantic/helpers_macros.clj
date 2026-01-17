(ns lexicon.semantic.helpers-macros)

(defmacro with-buffer
  "Execute body with a buffer as current buffer.

  Can be called with buffer name (creates if doesn't exist)."
  [buffer-name & body]
  `(let [buf-name# ~buffer-name
         ;; Find or create buffer
         buf-id# (or (let [buffers# (get-in @re-frame.db/app-db [:buffers])]
                       (->> buffers# vals
                            (filter #(= buf-name# (:name %)))
                            first :id))
                     (lexicon.semantic.helpers/create-buffer buf-name#))
         ;; Save old active window state
         old-tree# (get-in @re-frame.db/app-db [:window-tree])]
     ;; Make this buffer current by switching active window to display it
     (letfn [(update-active-window# [tree#]
               (cond
                 (nil? tree#) nil
                 (= (:type tree#) :leaf)
                 (if (= (:id tree#) (get-in @re-frame.db/app-db [:active-window-id]))
                   (assoc tree# :buffer-id buf-id#)
                   tree#)
                 :else
                 (assoc tree#
                        :first (update-active-window# (:first tree#))
                        :second (update-active-window# (:second tree#)))))]
       (swap! re-frame.db/app-db update :window-tree update-active-window#))
     (try
       ~@body
       (finally
         ;; Restore old window state
         (swap! re-frame.db/app-db assoc :window-tree old-tree#)))))

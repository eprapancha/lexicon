(ns lexicon.core.hi-lock
  "Hi-lock mode - Interactive regexp highlighting.

  Implements Emacs hi-lock.el functionality. Allows users to
  interactively highlight text matching regular expressions.

  Key commands:
  - M-s h r (highlight-regexp): Highlight matches of a regexp
  - M-s h p (highlight-phrase): Highlight a phrase
  - M-s h l (highlight-lines-matching-regexp): Highlight entire lines
  - M-s h u (unhighlight-regexp): Remove highlighting
  - M-s h . (highlight-symbol-at-point): Highlight symbol at point

  Based on Emacs hi-lock.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; =============================================================================
;; Faces for highlighting
;; =============================================================================

(def hi-lock-faces
  "Default faces for hi-lock highlighting.
   Emacs provides these predefined faces."
  [:hi-yellow :hi-pink :hi-green :hi-blue
   :hi-black-b :hi-blue-b :hi-red-b :hi-green-b :hi-black-hb])

(def face-index (atom 0))

(defn next-hi-lock-face
  "Get the next face in rotation for highlighting."
  []
  (let [face (nth hi-lock-faces (mod @face-index (count hi-lock-faces)))]
    (swap! face-index inc)
    face))

;; =============================================================================
;; State Management
;; =============================================================================

;; hi-lock patterns are stored per-buffer:
;; [:buffers buffer-id :hi-lock-patterns] = [{:regexp "..." :face :hi-yellow :id 1} ...]

(rf/reg-event-db
 :hi-lock/add-pattern
 (fn [db [_ buffer-id pattern]]
   "Add a hi-lock pattern to the buffer."
   (update-in db [:buffers buffer-id :hi-lock-patterns]
              (fnil conj []) pattern)))

(rf/reg-event-db
 :hi-lock/remove-pattern
 (fn [db [_ buffer-id pattern-id]]
   "Remove a hi-lock pattern from the buffer."
   (update-in db [:buffers buffer-id :hi-lock-patterns]
              (fn [patterns]
                (vec (remove #(= (:id %) pattern-id) patterns))))))

(rf/reg-event-db
 :hi-lock/clear-patterns
 (fn [db [_ buffer-id]]
   "Clear all hi-lock patterns from the buffer."
   (assoc-in db [:buffers buffer-id :hi-lock-patterns] [])))

;; =============================================================================
;; Overlay Management for Highlighting
;; =============================================================================

(defn- find-all-matches
  "Find all matches of a regexp in text.
   Returns a seq of {:start pos :end pos}."
  [text regexp-str]
  (try
    (let [re (js/RegExp. regexp-str "g")
          matches (atom [])]
      (loop []
        (when-let [match (.exec re text)]
          (let [start (.-index match)
                matched-text (aget match 0)
                end (+ start (count matched-text))]
            (swap! matches conj {:start start :end end})
            (recur))))
      @matches)
    (catch :default _
      ;; Invalid regexp
      [])))

;; =============================================================================
;; Hi-lock Commands
;; =============================================================================

(rf/reg-event-fx
 :hi-lock/highlight-regexp
 (fn [{:keys [db]} [_ regexp face]]
   "Highlight all matches of REGEXP with FACE in the current buffer."
   (let [active-window-id (:active-window-id db)
         window (get-in db [:window-tree])
         buffer-id (when window
                     (if (= (:type window) :leaf)
                       (:buffer-id window)
                       (let [w (first (filter #(= (:id %) active-window-id)
                                              (tree-seq map? #(concat [(:first %)] [(:second %)])
                                                        window)))]
                         (:buffer-id w))))
         pattern-id (inc (count (get-in db [:buffers buffer-id :hi-lock-patterns] [])))
         pattern {:id pattern-id
                  :regexp regexp
                  :face (or face (next-hi-lock-face))}]
     (if buffer-id
       {:db (update-in db [:buffers buffer-id :hi-lock-patterns]
                       (fnil conj []) pattern)
        :fx [[:dispatch-later {:ms 0 :dispatch [:hi-lock/apply-pattern buffer-id pattern]}]]}
       {:fx [[:dispatch [:echo/message "No buffer to highlight"]]]}))))

(rf/reg-event-fx
 :hi-lock/apply-pattern
 (fn [{:keys [db]} [_ buffer-id pattern]]
   "Apply a hi-lock pattern by creating overlays."
   (let [buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText wasm) (catch :default _ nil)))
         matches (when text (find-all-matches text (:regexp pattern)))
         face (:face pattern)]
     (if (seq matches)
       {:fx (vec (map (fn [match]
                        [:dispatch [:overlays/make buffer-id (:start match) (:end match)
                                    :face face
                                    :priority 10
                                    :hi-lock-pattern-id (:id pattern)]])
                      matches))}
       {:fx [[:dispatch [:echo/message (str "No matches for: " (:regexp pattern))]]]}))))

(rf/reg-event-fx
 :hi-lock/unhighlight-regexp
 (fn [{:keys [db]} [_ regexp-or-id]]
   "Remove highlighting for a pattern."
   (let [active-window-id (:active-window-id db)
         window (get-in db [:window-tree])
         buffer-id (when window
                     (if (= (:type window) :leaf)
                       (:buffer-id window)
                       (let [w (first (filter #(= (:id %) active-window-id)
                                              (tree-seq map? #(concat [(:first %)] [(:second %)])
                                                        window)))]
                         (:buffer-id w))))
         patterns (get-in db [:buffers buffer-id :hi-lock-patterns] [])
         ;; Find pattern by regexp string or by ID
         pattern (if (number? regexp-or-id)
                   (first (filter #(= (:id %) regexp-or-id) patterns))
                   (first (filter #(= (:regexp %) regexp-or-id) patterns)))]
     (if pattern
       (let [overlays (get-in db [:buffers buffer-id :overlays] {})
             overlay-ids-to-remove (keep (fn [[id o]]
                                           (when (= (:hi-lock-pattern-id o) (:id pattern))
                                             id))
                                         overlays)]
         {:db (-> db
                  (update-in [:buffers buffer-id :hi-lock-patterns]
                             (fn [ps] (vec (remove #(= (:id %) (:id pattern)) ps))))
                  (update-in [:buffers buffer-id :overlays]
                             (fn [os]
                               (reduce dissoc os overlay-ids-to-remove))))})
       {:fx [[:dispatch [:echo/message "No matching pattern to unhighlight"]]]}))))

;; =============================================================================
;; Interactive Commands
;; =============================================================================

(rf/reg-event-fx
 :command/highlight-regexp
 (fn [{:keys [db]} [_]]
   "Interactive highlight-regexp command."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Regexp to highlight: "
                      :on-confirm [:hi-lock/highlight-regexp-confirm]}]]]}))

(rf/reg-event-fx
 :hi-lock/highlight-regexp-confirm
 (fn [{:keys [db]} [_ regexp]]
   (if (not (str/blank? regexp))
     {:fx [[:dispatch [:hi-lock/highlight-regexp regexp nil]]
           [:dispatch [:minibuffer/deactivate]]
           [:dispatch [:echo/message (str "Highlighted: " regexp)]]]}
     {:fx [[:dispatch [:minibuffer/deactivate]]]})))

(rf/reg-event-fx
 :command/unhighlight-regexp
 (fn [{:keys [db]} [_]]
   "Interactive unhighlight-regexp command."
   (let [window (get-in db [:window-tree])
         buffer-id (when window
                     (if (= (:type window) :leaf)
                       (:buffer-id window)
                       nil))
         patterns (get-in db [:buffers buffer-id :hi-lock-patterns] [])]
     (if (empty? patterns)
       {:fx [[:dispatch [:echo/message "No patterns to unhighlight"]]]}
       {:fx [[:dispatch [:minibuffer/activate
                         {:prompt "Regexp to unhighlight: "
                          :completions (mapv :regexp patterns)
                          :on-confirm [:hi-lock/unhighlight-regexp-confirm]}]]]}))))

(rf/reg-event-fx
 :hi-lock/unhighlight-regexp-confirm
 (fn [{:keys [db]} [_ regexp]]
   {:fx [[:dispatch [:hi-lock/unhighlight-regexp regexp]]
         [:dispatch [:minibuffer/deactivate]]]}))

(rf/reg-event-fx
 :command/highlight-phrase
 (fn [{:keys [db]} [_]]
   "Interactive highlight-phrase command (highlights with word boundaries relaxed)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Phrase to highlight: "
                      :on-confirm [:hi-lock/highlight-phrase-confirm]}]]]}))

(rf/reg-event-fx
 :hi-lock/highlight-phrase-confirm
 (fn [{:keys [_db]} [_ phrase]]
   (if (not (str/blank? phrase))
     ;; Use the phrase directly - hi-lock/highlight-regexp handles the matching
     {:fx [[:dispatch [:hi-lock/highlight-regexp phrase nil]]
           [:dispatch [:minibuffer/deactivate]]
           [:dispatch [:echo/message (str "Highlighted phrase: " phrase)]]]}
     {:fx [[:dispatch [:minibuffer/deactivate]]]})))

(rf/reg-event-fx
 :command/highlight-symbol-at-point
 (fn [{:keys [db]} [_]]
   "Highlight the symbol at point."
   (let [window (get-in db [:window-tree])
         buffer-id (when window
                     (if (= (:type window) :leaf)
                       (:buffer-id window)
                       nil))
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText wasm) (catch :default _ "")))
         point (get-in db [:ui :cursor-position] 0)]
     (if text
       ;; Extract symbol at point
       (let [;; Find word boundaries around point
             before-text (subs text 0 (min point (count text)))
             after-text (subs text (min point (count text)))
             word-chars "\\w"
             start-match (re-find (re-pattern (str "[" word-chars "]*$")) before-text)
             end-match (re-find (re-pattern (str "^[" word-chars "]*")) after-text)
             symbol (str start-match end-match)]
         (if (not (str/blank? symbol))
           {:fx [[:dispatch [:hi-lock/highlight-regexp (str "\\b" symbol "\\b") nil]]
                 [:dispatch [:echo/message (str "Highlighted symbol: " symbol)]]]}
           {:fx [[:dispatch [:echo/message "No symbol at point"]]]}))
       {:fx [[:dispatch [:echo/message "No buffer text"]]]}))))

;; =============================================================================
;; Minor Mode
;; =============================================================================

(rf/reg-event-db
 :hi-lock/toggle-mode
 (fn [db [_ buffer-id]]
   "Toggle hi-lock-mode for a buffer."
   (update-in db [:buffers buffer-id :minor-modes]
              (fn [modes]
                (if (contains? modes :hi-lock-mode)
                  (disj modes :hi-lock-mode)
                  (conj (or modes #{}) :hi-lock-mode))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize hi-lock module and register commands."
  []
  ;; Register highlight-regexp command
  (rf/dispatch [:register-command :highlight-regexp
                {:docstring "Highlight text matching a regular expression"
                 :interactive "sRegexp to highlight: "
                 :handler [:command/highlight-regexp]}])

  ;; Register unhighlight-regexp command
  (rf/dispatch [:register-command :unhighlight-regexp
                {:docstring "Remove highlighting for a regular expression"
                 :interactive nil
                 :handler [:command/unhighlight-regexp]}])

  ;; Register highlight-phrase command
  (rf/dispatch [:register-command :highlight-phrase
                {:docstring "Highlight a phrase with flexible whitespace matching"
                 :interactive "sPhrase to highlight: "
                 :handler [:command/highlight-phrase]}])

  ;; Register highlight-symbol-at-point command
  (rf/dispatch [:register-command :highlight-symbol-at-point
                {:docstring "Highlight the symbol at point"
                 :interactive nil
                 :handler [:command/highlight-symbol-at-point]}])

  ;; Register hi-lock-mode command
  (rf/dispatch [:register-command :hi-lock-mode
                {:docstring "Toggle hi-lock minor mode for interactive highlighting"
                 :interactive nil
                 :handler [:hi-lock/toggle-mode]}]))

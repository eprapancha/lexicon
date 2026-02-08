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

  Based on Emacs hi-lock.el

  NOTE: This package uses only lisp.cljs primitives, making it suitable
  for extraction to an external package."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Faces for highlighting
;; =============================================================================

(def hi-lock-faces
  "Default faces for hi-lock highlighting.
   Emacs provides these predefined faces."
  [:hi-yellow :hi-pink :hi-green :hi-blue
   :hi-black-b :hi-blue-b :hi-red-b :hi-green-b :hi-black-hb])

(defonce face-index (atom 0))

(defn next-hi-lock-face
  "Get the next face in rotation for highlighting."
  []
  (let [face (nth hi-lock-faces (mod @face-index (count hi-lock-faces)))]
    (swap! face-index inc)
    face))

;; =============================================================================
;; Pattern Storage (per-buffer via buffer-local concept)
;; =============================================================================

;; Store patterns in a package-local atom keyed by buffer-id
;; {buffer-id [{:regexp "..." :face :hi-yellow :id 1 :overlay-ids [...]} ...]}
(defonce hi-lock-patterns (atom {}))

(defn get-patterns
  "Get hi-lock patterns for current buffer."
  []
  (get @hi-lock-patterns (lisp/current-buffer) []))

(defn add-pattern!
  "Add a pattern to current buffer."
  [pattern]
  (let [buffer-id (lisp/current-buffer)]
    (swap! hi-lock-patterns update buffer-id (fnil conj []) pattern)))

(defn remove-pattern!
  "Remove a pattern by ID from current buffer."
  [pattern-id]
  (let [buffer-id (lisp/current-buffer)]
    (swap! hi-lock-patterns update buffer-id
           (fn [patterns]
             (vec (remove #(= (:id %) pattern-id) patterns))))))

(defn clear-patterns!
  "Clear all patterns from current buffer."
  []
  (let [buffer-id (lisp/current-buffer)]
    (swap! hi-lock-patterns assoc buffer-id [])))

;; =============================================================================
;; Overlay Management for Highlighting
;; =============================================================================

(defn find-all-matches
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

(defn apply-highlighting!
  "Apply highlighting overlays for a pattern."
  [pattern]
  (let [text (lisp/buffer-string)
        matches (find-all-matches text (:regexp pattern))
        face (:face pattern)]
    (if (seq matches)
      (let [overlay-ids (doall
                         (map (fn [match]
                                (let [ov-id (lisp/make-overlay (:start match) (:end match))]
                                  (lisp/overlay-put ov-id :face face)
                                  (lisp/overlay-put ov-id :priority 10)
                                  (lisp/overlay-put ov-id :hi-lock-pattern-id (:id pattern))
                                  ov-id))
                              matches))]
        ;; Store overlay IDs with the pattern
        (let [buffer-id (lisp/current-buffer)]
          (swap! hi-lock-patterns update buffer-id
                 (fn [patterns]
                   (mapv (fn [p]
                           (if (= (:id p) (:id pattern))
                             (assoc p :overlay-ids (vec overlay-ids))
                             p))
                         patterns))))
        (lisp/message (str "Highlighted " (count matches) " matches for: " (:regexp pattern))))
      (lisp/message (str "No matches for: " (:regexp pattern))))))

(defn remove-highlighting!
  "Remove highlighting overlays for a pattern."
  [pattern]
  (doseq [ov-id (:overlay-ids pattern)]
    (lisp/delete-overlay ov-id)))

;; =============================================================================
;; Hi-lock Commands (using lisp.cljs primitives)
;; =============================================================================

(defn highlight-regexp!
  "Highlight all matches of REGEXP with FACE in the current buffer."
  [regexp face]
  (let [pattern-id (inc (count (get-patterns)))
        pattern {:id pattern-id
                 :regexp regexp
                 :face (or face (next-hi-lock-face))
                 :overlay-ids []}]
    (add-pattern! pattern)
    (apply-highlighting! pattern)))

(defn unhighlight-regexp!
  "Remove highlighting for a pattern (by regexp string or ID)."
  [regexp-or-id]
  (let [patterns (get-patterns)
        pattern (if (number? regexp-or-id)
                  (first (filter #(= (:id %) regexp-or-id) patterns))
                  (first (filter #(= (:regexp %) regexp-or-id) patterns)))]
    (if pattern
      (do
        (remove-highlighting! pattern)
        (remove-pattern! (:id pattern))
        (lisp/message (str "Unhighlighted: " (:regexp pattern))))
      (lisp/message "No matching pattern to unhighlight"))))

(defn highlight-symbol-at-point!
  "Highlight the symbol at point."
  []
  (if-let [symbol (lisp/symbol-at-point)]
    (do
      (highlight-regexp! (str "\\b" symbol "\\b") nil)
      (lisp/message (str "Highlighted symbol: " symbol)))
    (lisp/message "No symbol at point")))

;; =============================================================================
;; Re-frame Events (thin wrappers)
;; =============================================================================

(rf/reg-event-fx
 :hi-lock/highlight-regexp
 (fn [{:keys [db]} [_ regexp face]]
   (highlight-regexp! regexp face)
   {:db db}))

(rf/reg-event-fx
 :hi-lock/unhighlight-regexp
 (fn [{:keys [db]} [_ regexp-or-id]]
   (unhighlight-regexp! regexp-or-id)
   {:db db}))

(rf/reg-event-fx
 :command/highlight-regexp
 (fn [{:keys [db]} [_]]
   {:db db
    :fx [[:dispatch [:minibuffer/activate
                     {:prompt "Regexp to highlight: "
                      :on-confirm [:hi-lock/highlight-regexp-confirm]}]]]}))

(rf/reg-event-fx
 :hi-lock/highlight-regexp-confirm
 (fn [{:keys [db]} [_ regexp]]
   (if (not (str/blank? regexp))
     (do
       (highlight-regexp! regexp nil)
       {:db db
        :fx [[:dispatch [:minibuffer/deactivate]]]})
     {:db db
      :fx [[:dispatch [:minibuffer/deactivate]]]})))

(rf/reg-event-fx
 :command/unhighlight-regexp
 (fn [{:keys [db]} [_]]
   (let [patterns (get-patterns)]
     (if (empty? patterns)
       {:db db
        :fx [[:dispatch [:echo/message "No patterns to unhighlight"]]]}
       {:db db
        :fx [[:dispatch [:minibuffer/activate
                         {:prompt "Regexp to unhighlight: "
                          :completions (mapv :regexp patterns)
                          :on-confirm [:hi-lock/unhighlight-regexp-confirm]}]]]}))))

(rf/reg-event-fx
 :hi-lock/unhighlight-regexp-confirm
 (fn [{:keys [db]} [_ regexp]]
   (unhighlight-regexp! regexp)
   {:db db
    :fx [[:dispatch [:minibuffer/deactivate]]]}))

(rf/reg-event-fx
 :command/highlight-phrase
 (fn [{:keys [db]} [_]]
   {:db db
    :fx [[:dispatch [:minibuffer/activate
                     {:prompt "Phrase to highlight: "
                      :on-confirm [:hi-lock/highlight-phrase-confirm]}]]]}))

(rf/reg-event-fx
 :hi-lock/highlight-phrase-confirm
 (fn [{:keys [db]} [_ phrase]]
   (if (not (str/blank? phrase))
     (do
       (highlight-regexp! phrase nil)
       {:db db
        :fx [[:dispatch [:minibuffer/deactivate]]]})
     {:db db
      :fx [[:dispatch [:minibuffer/deactivate]]]})))

(rf/reg-event-fx
 :command/highlight-symbol-at-point
 (fn [{:keys [db]} [_]]
   (highlight-symbol-at-point!)
   {:db db}))

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
                 :handler [:command/highlight-symbol-at-point]}]))

(ns lexicon.packages.hi-lock
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

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
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

;; Track hi-lock-mode enabled state per buffer
(defonce hi-lock-mode-enabled (atom #{}))

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
;; Hi-lock Commands
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

(defn hi-lock-mode!
  "Toggle hi-lock-mode for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        enabled? (contains? @hi-lock-mode-enabled buffer-id)]
    (if enabled?
      (do
        (swap! hi-lock-mode-enabled disj buffer-id)
        (lisp/message "Hi-lock mode disabled"))
      (do
        (swap! hi-lock-mode-enabled conj buffer-id)
        (lisp/message "Hi-lock mode enabled")))))

;; =============================================================================
;; Interactive Command Handlers
;; =============================================================================

(defn highlight-regexp-interactive []
  (lisp/read-from-minibuffer
   "Regexp to highlight: "
   (fn [regexp]
     (when (not (str/blank? regexp))
       (highlight-regexp! regexp nil)))))

(defn unhighlight-regexp-interactive []
  (let [patterns (get-patterns)]
    (if (empty? patterns)
      (lisp/message "No patterns to unhighlight")
      (lisp/read-from-minibuffer
       "Regexp to unhighlight: "
       (fn [regexp]
         (unhighlight-regexp! regexp))))))

(defn highlight-phrase-interactive []
  (lisp/read-from-minibuffer
   "Phrase to highlight: "
   (fn [phrase]
     (when (not (str/blank? phrase))
       (highlight-regexp! phrase nil)))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-hi-lock!
  "Initialize hi-lock module and register commands."
  []
  (lisp/define-command 'highlight-regexp
    highlight-regexp-interactive
    "Highlight text matching a regular expression")

  (lisp/define-command 'unhighlight-regexp
    unhighlight-regexp-interactive
    "Remove highlighting for a regular expression")

  (lisp/define-command 'highlight-phrase
    highlight-phrase-interactive
    "Highlight a phrase with flexible whitespace matching")

  (lisp/define-command 'highlight-symbol-at-point
    highlight-symbol-at-point!
    "Highlight the symbol at point")

  (lisp/define-command 'hi-lock-mode
    hi-lock-mode!
    "Toggle hi-lock mode for current buffer")

  ;; Register keybindings for hi-lock (M-s h prefix)
  (lisp/global-set-key "M-s h r" :highlight-regexp)
  (lisp/global-set-key "M-s h p" :highlight-phrase)
  (lisp/global-set-key "M-s h u" :unhighlight-regexp)
  (lisp/global-set-key "M-s h ." :highlight-symbol-at-point))

(ns lexicon.core.modes.show-paren
  "Show Paren Mode - Highlight matching parentheses.

  When enabled, highlights the matching paren when point is on or after
  a parenthesis, bracket, or brace. Provides visual feedback for balanced
  delimiters."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]
            [clojure.string :as str]))

;; =============================================================================
;; Paren Matching Logic
;; =============================================================================

(def opening-parens #{\( \[ \{})
(def closing-parens #{\) \] \}})
(def paren-pairs {\( \), \[ \], \{ \}
                  \) \(, \] \[, \} \{})

(defn opening-paren? [ch] (contains? opening-parens ch))
(defn closing-paren? [ch] (contains? closing-parens ch))
(defn paren? [ch] (or (opening-paren? ch) (closing-paren? ch)))

(defn find-matching-paren-forward
  "Find matching closing paren starting from pos (exclusive).
  Returns position of matching paren or nil."
  [text pos opening-char]
  (let [closing-char (get paren-pairs opening-char)
        len (count text)]
    (loop [i (inc pos)
           depth 1]
      (when (< i len)
        (let [ch (nth text i)]
          (cond
            (= ch opening-char)
            (recur (inc i) (inc depth))

            (= ch closing-char)
            (if (= depth 1)
              i  ; Found match
              (recur (inc i) (dec depth)))

            :else
            (recur (inc i) depth)))))))

(defn find-matching-paren-backward
  "Find matching opening paren searching backward from pos (exclusive).
  Returns position of matching paren or nil."
  [text pos closing-char]
  (let [opening-char (get paren-pairs closing-char)]
    (loop [i (dec pos)
           depth 1]
      (when (>= i 0)
        (let [ch (nth text i)]
          (cond
            (= ch closing-char)
            (recur (dec i) (inc depth))

            (= ch opening-char)
            (if (= depth 1)
              i  ; Found match
              (recur (dec i) (dec depth)))

            :else
            (recur (dec i) depth)))))))

(defn find-paren-match
  "Find matching paren for character at or before point.
  Returns {:paren-pos pos :match-pos pos :matched? bool} or nil if no paren."
  [text cursor-pos]
  (when (and text (> (count text) 0) (>= cursor-pos 0))
    (let [;; Check char at cursor and char before cursor
          char-at (when (< cursor-pos (count text)) (nth text cursor-pos))
          char-before (when (> cursor-pos 0) (nth text (dec cursor-pos)))]
      (cond
        ;; Cursor is on an opening paren
        (and char-at (opening-paren? char-at))
        (let [match-pos (find-matching-paren-forward text cursor-pos char-at)]
          {:paren-pos cursor-pos
           :match-pos match-pos
           :matched? (some? match-pos)})

        ;; Cursor is after a closing paren (Emacs style)
        (and char-before (closing-paren? char-before))
        (let [paren-pos (dec cursor-pos)
              match-pos (find-matching-paren-backward text paren-pos char-before)]
          {:paren-pos paren-pos
           :match-pos match-pos
           :matched? (some? match-pos)})

        ;; Cursor is on a closing paren (also check this case)
        (and char-at (closing-paren? char-at))
        (let [match-pos (find-matching-paren-backward text cursor-pos char-at)]
          {:paren-pos cursor-pos
           :match-pos match-pos
           :matched? (some? match-pos)})

        ;; No paren at or near cursor
        :else nil))))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-show-paren-mode!
  "Initialize show-paren-mode."
  []
  (modes/define-minor-mode
   :show-paren-mode
   "Toggle highlighting of matching parentheses"
   {:lighter " Paren"
    :buffer-local? true
    :init-value false}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn show-paren-enabled?
  "Check if show-paren-mode is enabled for the active buffer."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (contains? (get-in db [:buffers buffer-id :minor-modes]) :show-paren-mode)))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :show-paren/enabled?
 (fn [db _]
   (show-paren-enabled? db)))

(rf/reg-sub
 :show-paren/match-info
 (fn [db _]
   "Get matching paren info for current cursor position.
    Returns {:paren-pos :match-pos :matched?} or nil."
   (when (show-paren-enabled? db)
     (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
           buffer-id (:buffer-id active-window)
           buffer (get-in db [:buffers buffer-id])
           wasm-instance (:wasm-instance buffer)
           cursor-linear (get-in db [:ui :cursor-position] 0)]
       (when wasm-instance
         (let [text (try (.getText wasm-instance) (catch :default _ nil))]
           (when text
             (find-paren-match text cursor-linear))))))))

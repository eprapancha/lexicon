(ns lexicon.packages.dabbrev
  "Dynamic abbreviation expansion (dabbrev).

  M-/ expands the word before point to a matching word found elsewhere
  in the buffer. Repeated M-/ cycles through possible expansions.

  Based on Emacs lisp/dabbrev.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State for cycling through expansions
;; =============================================================================

(defonce last-abbreviation (atom nil))
(defonce expansion-table (atom #{}))
(defonce last-expansion (atom nil))
(defonce abbrev-start-pos (atom nil))
(defonce search-state (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- word-char? [ch]
  (when ch (re-matches #"[\w]" (str ch))))

(defn- get-abbrev-at-point [text cursor-pos]
  (when (and text (pos? cursor-pos) (<= cursor-pos (count text)))
    (let [before-cursor (subs text 0 cursor-pos)]
      (loop [pos (dec (count before-cursor))]
        (if (neg? pos)
          (when (pos? (count before-cursor))
            {:abbrev before-cursor :start 0})
          (let [ch (nth before-cursor pos)]
            (if (word-char? ch)
              (recur (dec pos))
              (let [abbrev (subs before-cursor (inc pos))]
                (when (pos? (count abbrev))
                  {:abbrev abbrev :start (inc pos)})))))))))

(defn- find-expansion-backward [text abbrev from-pos already-found]
  (let [abbrev-lower (str/lower-case abbrev)
        abbrev-len (count abbrev)]
    (loop [pos (dec from-pos)]
      (when (>= pos 0)
        (let [at-word-start? (and (word-char? (nth text pos nil))
                                  (or (zero? pos)
                                      (not (word-char? (nth text (dec pos) nil)))))]
          (if at-word-start?
            (let [word-end (loop [end pos]
                             (if (and (< end (count text))
                                      (word-char? (nth text end nil)))
                               (recur (inc end))
                               end))
                  word (subs text pos word-end)
                  word-lower (str/lower-case word)]
              (if (and (> (count word) abbrev-len)
                       (str/starts-with? word-lower abbrev-lower)
                       (not (contains? already-found word-lower)))
                {:expansion word :position pos}
                (recur (dec pos))))
            (recur (dec pos))))))))

(defn- find-expansion-forward [text abbrev from-pos already-found]
  (let [abbrev-lower (str/lower-case abbrev)
        abbrev-len (count abbrev)
        text-len (count text)]
    (loop [pos from-pos]
      (when (< pos text-len)
        (let [at-word-start? (and (word-char? (nth text pos nil))
                                  (or (zero? pos)
                                      (not (word-char? (nth text (dec pos) nil)))))]
          (if at-word-start?
            (let [word-end (loop [end pos]
                             (if (and (< end text-len)
                                      (word-char? (nth text end nil)))
                               (recur (inc end))
                               end))
                  word (subs text pos word-end)
                  word-lower (str/lower-case word)]
              (if (and (> (count word) abbrev-len)
                       (str/starts-with? word-lower abbrev-lower)
                       (not (contains? already-found word-lower)))
                {:expansion word :position pos}
                (recur (inc word-end))))
            (recur (inc pos))))))))

(defn- reset-dabbrev-state! []
  (reset! last-abbreviation nil)
  (reset! expansion-table #{})
  (reset! last-expansion nil)
  (reset! abbrev-start-pos nil)
  (reset! search-state nil))

(defn- apply-case-pattern [abbrev expansion]
  (cond
    (= abbrev (str/upper-case abbrev))
    (str/upper-case expansion)

    (and (pos? (count abbrev))
         (let [first-char (subs abbrev 0 1)]
           (= first-char (str/upper-case first-char)))
         (= (subs abbrev 1) (str/lower-case (subs abbrev 1))))
    (str (str/upper-case (subs expansion 0 1)) (subs expansion 1))

    :else expansion))

;; =============================================================================
;; Main Expansion Logic
;; =============================================================================

(defn dabbrev-expand!
  "Expand the abbreviation before point (M-/).
   On repeated calls, cycle through possible expansions."
  []
  (let [text (lisp/buffer-string)
        cursor-pos (lisp/point)
        last-cmd (lisp/last-command)]

    (if (and (= last-cmd :dabbrev-expand)
             @last-abbreviation
             @abbrev-start-pos)
      ;; Continuation - find next expansion
      (let [abbrev @last-abbreviation
            current-expansion @last-expansion
            search @search-state
            already-found @expansion-table
            original-end (+ @abbrev-start-pos (count abbrev))
            result (case (:direction search)
                     :backward
                     (or (find-expansion-backward text abbrev (:position search) already-found)
                         (do (swap! search-state assoc :direction :forward :position original-end)
                             (find-expansion-forward text abbrev original-end already-found)))
                     :forward
                     (find-expansion-forward text abbrev (:position search) already-found)
                     (find-expansion-backward text abbrev @abbrev-start-pos already-found))]

        (if result
          (let [{:keys [expansion position]} result
                cased-expansion (apply-case-pattern abbrev expansion)]
            (swap! expansion-table conj (str/lower-case expansion))
            (reset! last-expansion cased-expansion)
            (swap! search-state assoc :position position)
            (lisp/replace-text @abbrev-start-pos
                               (count (or current-expansion abbrev))
                               cased-expansion)
            (lisp/message (str "Expansion: " cased-expansion)))
          (lisp/message "No further expansions")))

      ;; New expansion
      (let [abbrev-info (get-abbrev-at-point text cursor-pos)]
        (if-not abbrev-info
          (lisp/message "No abbreviation at point")
          (let [{:keys [abbrev start]} abbrev-info
                result (or (find-expansion-backward text abbrev cursor-pos #{})
                           (find-expansion-forward text abbrev cursor-pos #{}))]
            (if result
              (let [{:keys [expansion position]} result
                    cased-expansion (apply-case-pattern abbrev expansion)
                    direction (if (< position cursor-pos) :backward :forward)]
                (reset! last-abbreviation abbrev)
                (reset! expansion-table #{(str/lower-case expansion)})
                (reset! last-expansion cased-expansion)
                (reset! abbrev-start-pos start)
                (reset! search-state {:direction direction :position position})
                (lisp/replace-text start (count abbrev) cased-expansion)
                (lisp/message (str "Expansion: " cased-expansion)))
              (lisp/message (str "No expansion for \"" abbrev "\"")))))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-dabbrev!
  "Initialize dabbrev commands and keybindings."
  []
  (lisp/define-command
    'dabbrev-expand
    dabbrev-expand!
    "Expand the word before point by searching for matching words (M-/)")

  (lisp/global-set-key "M-/" :dabbrev-expand))

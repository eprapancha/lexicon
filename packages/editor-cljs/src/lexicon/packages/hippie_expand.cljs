(ns lexicon.packages.hippie-expand
  "Hippie expand - try multiple expansion strategies.

  M-/ (or hippie-expand) tries various ways to complete the text before point.
  The list of expansion strategies is defined in `hippie-expand-try-functions-list`.

  Repeated calls cycle through possible expansions from all strategies.

  Based on Emacs lisp/hippie-exp.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def default-try-functions
  [:try-expand-dabbrev
   :try-expand-dabbrev-all-buffers
   :try-expand-line
   :try-complete-file-name])

;; =============================================================================
;; State
;; =============================================================================

(defonce current-try-index (atom 0))
(defonce hippie-expand-active? (atom false))
(defonce original-text (atom nil))
(defonce expand-start-pos (atom nil))
(defonce tried-expansions (atom #{}))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- get-word-before-point [text cursor-pos]
  (when (and text (pos? cursor-pos) (<= cursor-pos (count text)))
    (let [before-cursor (subs text 0 cursor-pos)]
      (loop [pos (dec (count before-cursor))]
        (if (neg? pos)
          {:word before-cursor :start 0}
          (let [ch (nth before-cursor pos)]
            (if (re-matches #"[\w\-_]" (str ch))
              (recur (dec pos))
              {:word (subs before-cursor (inc pos))
               :start (inc pos)})))))))

(defn- escape-regex [s]
  (str/replace s #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn- find-dabbrev-expansion [text word cursor-pos already-tried direction]
  (let [pattern (re-pattern (str "\\b" (escape-regex word) "(\\w+)"))
        search-text (if (= direction :backward)
                      (subs text 0 cursor-pos)
                      (subs text cursor-pos))]
    (when-let [matches (re-seq pattern search-text)]
      (let [expansions (->> matches
                            (map (fn [[_ suffix]] (str word suffix)))
                            (remove already-tried)
                            distinct)]
        (when (seq expansions)
          {:expansion (first expansions)
           :remaining (rest expansions)})))))

;; =============================================================================
;; Try Functions
;; =============================================================================

(defn- try-expand-dabbrev [already-tried]
  (let [text (lisp/buffer-string)
        cursor-pos (lisp/point)]
    (when-let [{:keys [word start]} (get-word-before-point text cursor-pos)]
      (when (and word (>= (count word) 1))
        (or (find-dabbrev-expansion text word start already-tried :backward)
            (find-dabbrev-expansion text word cursor-pos already-tried :forward))))))

(defn- try-expand-dabbrev-all-buffers [already-tried]
  (let [text (lisp/buffer-string)
        cursor-pos (lisp/point)
        current-id (lisp/current-buffer)]
    (when-let [{:keys [word]} (get-word-before-point text cursor-pos)]
      (when (and word (>= (count word) 1))
        (let [all-ids (lisp/buffer-list-ids)
              other-ids (remove #(= % current-id) all-ids)]
          (some (fn [buf-id]
                  (let [buf-text (lisp/buffer-text-of buf-id)]
                    (when buf-text
                      (find-dabbrev-expansion buf-text word 0 already-tried :forward))))
                other-ids))))))

(defn- try-expand-line [already-tried]
  (let [text (lisp/buffer-string)
        cursor-pos (lisp/point)]
    (when text
      (let [line-start (loop [pos (dec cursor-pos)]
                         (if (or (neg? pos) (= (nth text pos) \newline))
                           (inc pos)
                           (recur (dec pos))))
            current-line-prefix (subs text line-start cursor-pos)
            lines (str/split-lines text)]
        (when (and current-line-prefix (seq current-line-prefix))
          (let [matching-lines (->> lines
                                    (filter #(and (str/starts-with? % current-line-prefix)
                                                  (not= % current-line-prefix)
                                                  (not (contains? already-tried %))))
                                    distinct)]
            (when (seq matching-lines)
              {:expansion (first matching-lines)
               :line-expansion? true})))))))

(defn- try-complete-file-name [_already-tried]
  nil)

(defn- dispatch-try-function [try-fn already-tried]
  (case try-fn
    :try-expand-dabbrev (try-expand-dabbrev already-tried)
    :try-expand-dabbrev-all-buffers (try-expand-dabbrev-all-buffers already-tried)
    :try-expand-line (try-expand-line already-tried)
    :try-complete-file-name (try-complete-file-name already-tried)
    nil))

;; =============================================================================
;; Main Logic
;; =============================================================================

(defn- reset-hippie-state! []
  (reset! current-try-index 0)
  (reset! hippie-expand-active? false)
  (reset! original-text nil)
  (reset! expand-start-pos nil)
  (reset! tried-expansions #{}))

(defn hippie-expand!
  "Try to expand text before point using various strategies."
  []
  (let [last-cmd (lisp/last-command)
        continuing? (= last-cmd :hippie-expand)
        text (lisp/buffer-string)
        cursor-pos (lisp/point)
        try-functions default-try-functions]

    (when-not continuing?
      (reset-hippie-state!)
      (reset! hippie-expand-active? true)
      (when-let [{:keys [word start]} (get-word-before-point text cursor-pos)]
        (reset! original-text word)
        (reset! expand-start-pos start)))

    (loop [idx @current-try-index]
      (if (>= idx (count try-functions))
        (do
          (reset-hippie-state!)
          (lisp/message "No expansion found"))

        (let [try-fn (nth try-functions idx)
              result (dispatch-try-function try-fn @tried-expansions)]
          (if result
            (let [{:keys [expansion line-expansion?]} result
                  word-info (get-word-before-point text cursor-pos)
                  start-pos (if line-expansion?
                              (loop [pos (dec cursor-pos)]
                                (if (or (neg? pos) (= (nth text pos) \newline))
                                  (inc pos)
                                  (recur (dec pos))))
                              (:start word-info))
                  delete-length (- cursor-pos start-pos)]
              (swap! tried-expansions conj expansion)
              (reset! current-try-index idx)
              (lisp/replace-text start-pos delete-length expansion)
              (lisp/message (str "Expansion: " expansion " [" (name try-fn) "]")))

            (do
              (swap! current-try-index inc)
              (recur (inc idx)))))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize hippie-expand commands."
  []
  (lisp/define-command
    'hippie-expand
    hippie-expand!
    "Expand text trying various methods (M-/)"))

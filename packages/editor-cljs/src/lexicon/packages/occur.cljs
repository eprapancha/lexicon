(ns lexicon.packages.occur
  "Occur mode - list all lines matching a regexp.

  M-x occur prompts for a regexp and creates an *Occur* buffer
  showing all matching lines with line numbers.

  Keybindings in *Occur* buffer:
  - RET: Jump to match in source buffer
  - n: Move to next match line
  - p: Move to previous match line
  - q: Quit occur buffer

  Based on Emacs lisp/replace.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State
;; =============================================================================

(defonce occur-source-buffer (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- format-line-num [fmt & args]
  (reduce (fn [s arg]
            (str/replace-first s #"%\d*d" (str arg)))
          fmt args))

(defn- find-matching-lines [text regexp-str]
  (try
    (let [lines (str/split-lines text)
          re (js/RegExp. regexp-str "gi")]
      (vec
       (keep-indexed
        (fn [idx line]
          (let [matches (loop [result [] last-index 0]
                          (set! (.-lastIndex re) last-index)
                          (if-let [m (.exec re line)]
                            (let [match-start (.-index m)
                                  match-text (aget m 0)
                                  match-end (+ match-start (count match-text))]
                              (recur (conj result [match-start match-end]) match-end))
                            result))]
            (when (seq matches)
              {:line-num (inc idx)
               :text line
               :match-ranges matches})))
        lines)))
    (catch :default _ [])))

(defn- format-occur-buffer [matches regexp source-buffer-name]
  (let [header (str (count matches) " matches for \"" regexp "\" in buffer: " source-buffer-name "\n")
        match-lines (vec (range 2 (+ 2 (count matches))))
        prefix-len 8
        formatted-lines (map-indexed
                         (fn [idx {:keys [line-num text match-ranges]}]
                           {:formatted (str (format-line-num "%6d: " line-num) text)
                            :occur-line (+ 2 idx)
                            :ranges (mapv (fn [[start end]]
                                           [(+ prefix-len start) (+ prefix-len end)])
                                         match-ranges)})
                         matches)
        content (str header "\n"
                     (str/join "\n" (map :formatted formatted-lines)))
        highlights (mapv (fn [{:keys [occur-line ranges]}]
                          {:line occur-line :ranges ranges})
                        formatted-lines)]
    {:content content
     :match-lines match-lines
     :highlights highlights}))

(defn- get-current-occur-line []
  (let [text (lisp/buffer-string)
        pos (lisp/point)
        lines-before (when text (subs text 0 (min pos (count text))))]
    (if lines-before
      (count (filter #(= % \newline) lines-before))
      0)))

(defn- find-next-match-line [match-lines current-line]
  (first (filter #(> % current-line) match-lines)))

(defn- find-prev-match-line [match-lines current-line]
  (last (filter #(< % current-line) match-lines)))

(defn- line-start-position [text line-num]
  (let [lines (str/split-lines text)]
    (if (zero? line-num)
      0
      (reduce + (map #(inc (count %)) (take line-num lines))))))

(defn- get-source-line-at-occur-line [occur-line]
  (when-let [source @occur-source-buffer]
    (let [matches (:matches source)
          match-index (- occur-line 2)]
      (when (and (>= match-index 0) (< match-index (count matches)))
        (:line-num (nth matches match-index))))))

;; =============================================================================
;; Commands
;; =============================================================================

(defn occur-goto-first-match! []
  (when-let [source @occur-source-buffer]
    (let [match-lines (:match-lines source)]
      (when (seq match-lines)
        (let [text (lisp/buffer-string)
              first-match-line (first match-lines)
              pos (line-start-position text first-match-line)]
          (lisp/goto-char pos))))))

(defn occur-goto-match! [line-num]
  (when-let [source @occur-source-buffer]
    (let [buffer-id (:buffer-id source)]
      (lisp/switch-to-buffer buffer-id)
      (lisp/goto-line line-num))))

(defn occur-next-line! []
  (if-let [source @occur-source-buffer]
    (let [match-lines (:match-lines source)
          current-line (get-current-occur-line)
          next-line (find-next-match-line match-lines current-line)]
      (if next-line
        (let [text (lisp/buffer-string)
              pos (line-start-position text next-line)]
          (lisp/goto-char pos))
        (lisp/message "No more matches")))
    nil))

(defn occur-prev-line! []
  (if-let [source @occur-source-buffer]
    (let [match-lines (:match-lines source)
          current-line (get-current-occur-line)
          prev-line (find-prev-match-line match-lines current-line)]
      (if prev-line
        (let [text (lisp/buffer-string)
              pos (line-start-position text prev-line)]
          (lisp/goto-char pos))
        (lisp/message "No previous matches")))
    nil))

(defn occur-quit! []
  (if-let [source @occur-source-buffer]
    (do
      (lisp/delete-window)
      (lisp/switch-to-buffer (:buffer-id source)))
    (lisp/delete-window)))

(defn occur-goto-match-at-point! []
  (if (= (lisp/buffer-name) "*Occur*")
    (let [current-line (get-current-occur-line)]
      (if-let [source-line (get-source-line-at-occur-line current-line)]
        (occur-goto-match! source-line)
        (when-let [source @occur-source-buffer]
          (when-let [first-match (first (:matches source))]
            (occur-goto-match! (:line-num first-match))))))
    (lisp/message "Not in *Occur* buffer")))

(defn occur-run! [regexp]
  (let [source-buffer-id (lisp/current-buffer)
        source-window-id nil  ; Not tracking window for simplified version
        buffer-name (lisp/buffer-name)
        text (lisp/buffer-string)
        matches (find-matching-lines (or text "") regexp)]

    (if (empty? matches)
      (lisp/message (str "No matches for \"" regexp "\""))

      (let [{:keys [content match-lines highlights]} (format-occur-buffer matches regexp buffer-name)
            enhanced-matches (map-indexed
                              (fn [idx m] (assoc m :occur-line (+ 2 idx)))
                              matches)

            ;; Create or reuse *Occur* buffer
            existing-id (lisp/get-buffer "*Occur*")
            _occur-buffer-id (if existing-id
                               (do
                                 (lisp/kill-buffer existing-id)
                                 (lisp/create-special-buffer "*Occur*" content
                                                             {:major-mode :occur-mode
                                                              :read-only true
                                                              :properties {:occur-highlights highlights}}))
                               (lisp/create-special-buffer "*Occur*" content
                                                           {:major-mode :occur-mode
                                                            :read-only true
                                                            :properties {:occur-highlights highlights}}))]

        (reset! occur-source-buffer {:buffer-id source-buffer-id
                                     :buffer-name buffer-name
                                     :window-id source-window-id
                                     :regexp regexp
                                     :matches (vec enhanced-matches)
                                     :match-lines match-lines})

        (lisp/split-window-below)
        (lisp/switch-to-buffer "*Occur*")
        (occur-goto-first-match!)
        (lisp/message (str (count matches) " matches for \"" regexp "\""))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-occur! []
  (lisp/define-command 'occur
    (fn []
      (lisp/read-from-minibuffer
       "List lines matching regexp: "
       occur-run!))
    "Show all lines matching REGEXP")

  (lisp/define-command 'occur-goto-match-at-point
    occur-goto-match-at-point!
    "Go to the occurrence on this line")

  (lisp/define-command 'occur-next-line
    occur-next-line!
    "Move to next match in occur buffer")

  (lisp/define-command 'occur-prev-line
    occur-prev-line!
    "Move to previous match in occur buffer")

  (lisp/define-command 'occur-quit
    occur-quit!
    "Quit occur buffer")

  (lisp/define-key-for-mode :occur-mode "RET" :occur-goto-match-at-point)
  (lisp/define-key-for-mode :occur-mode "n" :occur-next-line)
  (lisp/define-key-for-mode :occur-mode "p" :occur-prev-line)
  (lisp/define-key-for-mode :occur-mode "q" :occur-quit))

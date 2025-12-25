(ns lexicon.thing-at-point
  "Thing-at-point subsystem for context-aware operations.

  Identifies semantic units around point: symbols, URLs, filenames, words, etc.

  Used by:
  - Embark for context-aware actions
  - find-file-at-point (ffap)
  - help-mode for symbol lookup
  - completion systems

  Thing types:
  - :symbol - Elisp-style symbols (alphanumeric + hyphens/underscores)
  - :word - Words (alphanumeric only)
  - :url - URLs (http://, https://, ftp://, etc.)
  - :filename - File paths (/path/to/file, ./relative, ~/home)
  - :sexp - S-expression at point
  - :line - Current line
  - :sentence - Sentence at point
  - :email - Email address

  Usage:
    (thing-at-point db buffer-id point :symbol)  ; => 'my-function'
    (bounds-of-thing-at-point db buffer-id point :url) ; => {:start 10 :end 30}"
  (:require [clojure.string :as str]))

;; -- Boundary Detection Functions --

(defn get-buffer-text
  "Get full text from buffer at BUFFER-ID."
  [buffer-state]
  (let [wasm (:wasm-instance buffer-state)]
    (if wasm
      (try (.getText wasm) (catch js/Error _e ""))
      "")))

(defn char-at
  "Get character at POS in TEXT."
  [text pos]
  (when (and (>= pos 0) (< pos (count text)))
    (nth text pos)))

;; Symbol boundaries (alphanumeric + - _)
(defn symbol-char?
  "Check if CH is a symbol character."
  [ch]
  (and ch (re-matches #"[a-zA-Z0-9\-_]" (str ch))))

(defn bounds-of-symbol-at-point
  "Find symbol boundaries at POINT in BUFFER-STATE.
  Returns {:start N :end M} or nil."
  [buffer-state point]
  (let [text (get-buffer-text buffer-state)]
    (when (and (pos? (count text)) (< point (count text)))
      (let [;; Find start: search backward
            start (loop [i point]
                    (cond
                      (<= i 0) 0
                      (symbol-char? (char-at text (dec i))) (recur (dec i))
                      :else i))
            ;; Find end: search forward
            end (loop [i point]
                  (cond
                    (>= i (count text)) (count text)
                    (symbol-char? (char-at text i)) (recur (inc i))
                    :else i))]
        (when (< start end)
          {:start start :end end})))))

;; Word boundaries (alphanumeric only)
(defn word-char?
  "Check if CH is a word character."
  [ch]
  (and ch (re-matches #"[a-zA-Z0-9]" (str ch))))

(defn bounds-of-word-at-point
  "Find word boundaries at POINT in BUFFER-STATE."
  [buffer-state point]
  (let [text (get-buffer-text buffer-state)]
    (when (and (pos? (count text)) (< point (count text)))
      (let [start (loop [i point]
                    (cond
                      (<= i 0) 0
                      (word-char? (char-at text (dec i))) (recur (dec i))
                      :else i))
            end (loop [i point]
                  (cond
                    (>= i (count text)) (count text)
                    (word-char? (char-at text i)) (recur (inc i))
                    :else i))]
        (when (< start end)
          {:start start :end end})))))

;; URL boundaries
(def url-schemes
  "Supported URL schemes."
  #{"http" "https" "ftp" "ftps" "file" "mailto"})

(defn url-char?
  "Check if CH is valid in a URL."
  [ch]
  (and ch (re-matches #"[a-zA-Z0-9\-._~:/?#\[\]@!$&'()*+,;=%]" (str ch))))

(defn bounds-of-url-at-point
  "Find URL boundaries at POINT in BUFFER-STATE.
  Looks for http://, https://, etc."
  [buffer-state point]
  (let [text (get-buffer-text buffer-state)
        ;; Search backward for scheme
        search-start (max 0 (- point 20))  ; Look back up to 20 chars
        search-text (subs text search-start (min (count text) (+ point 100)))]
    (when-let [match (re-find #"(https?|ftp|ftps|file|mailto)://[a-zA-Z0-9\-._~:/?#\[\]@!$&'()*+,;=%]+" search-text)]
      (let [match-start (+ search-start (str/index-of search-text match))
            match-end (+ match-start (count match))]
        (when (and (>= point match-start) (<= point match-end))
          {:start match-start :end match-end})))))

;; Filename boundaries
(defn filename-char?
  "Check if CH is valid in a filename."
  [ch]
  (and ch (re-matches #"[a-zA-Z0-9/.\-_~]" (str ch))))

(defn bounds-of-filename-at-point
  "Find filename boundaries at POINT in BUFFER-STATE.
  Handles absolute (/path), relative (./path), and home (~/path) paths."
  [buffer-state point]
  (let [text (get-buffer-text buffer-state)]
    (when (and (pos? (count text)) (< point (count text)))
      ;; Look for path indicators: /, ./, ~/
      (let [search-start (max 0 (- point 100))
            search-end (min (count text) (+ point 100))
            search-text (subs text search-start search-end)
            ;; Match file paths
            pattern #"(?:~?/|\./)([a-zA-Z0-9/.\-_~]+)"
            matches (re-seq pattern search-text)]
        (when-let [match (first (filter (fn [[full-match _]]
                                         (let [abs-start (+ search-start (str/index-of search-text full-match))
                                               abs-end (+ abs-start (count full-match))]
                                           (and (>= point abs-start) (<= point abs-end))))
                                       matches))]
          (let [full-match (first match)
                match-start (+ search-start (str/index-of search-text full-match))
                match-end (+ match-start (count full-match))]
            {:start match-start :end match-end}))))))

;; Line boundaries
(defn bounds-of-line-at-point
  "Find line boundaries at POINT in BUFFER-STATE."
  [buffer-state point]
  (let [text (get-buffer-text buffer-state)]
    (when (pos? (count text))
      (let [;; Find start: search backward for newline
            start (loop [i point]
                    (cond
                      (<= i 0) 0
                      (= (char-at text (dec i)) \newline) i
                      :else (recur (dec i))))
            ;; Find end: search forward for newline
            end (loop [i point]
                  (cond
                    (>= i (count text)) (count text)
                    (= (char-at text i) \newline) i
                    :else (recur (inc i))))]
        {:start start :end end}))))

;; Email boundaries
(defn bounds-of-email-at-point
  "Find email address boundaries at POINT in BUFFER-STATE."
  [buffer-state point]
  (let [text (get-buffer-text buffer-state)
        search-start (max 0 (- point 50))
        search-end (min (count text) (+ point 50))
        search-text (subs text search-start search-end)
        ;; Simple email pattern
        pattern #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
        matches (re-seq pattern search-text)]
    (when-let [match (first (filter (fn [email]
                                     (let [abs-start (+ search-start (str/index-of search-text email))
                                           abs-end (+ abs-start (count email))]
                                       (and (>= point abs-start) (<= point abs-end))))
                                   matches))]
      (let [match-start (+ search-start (str/index-of search-text match))
            match-end (+ match-start (count match))]
        {:start match-start :end match-end}))))

;; -- Thing Type Dispatch --

(def thing-bounds-functions
  "Map of thing types to their boundary detection functions."
  {:symbol bounds-of-symbol-at-point
   :word bounds-of-word-at-point
   :url bounds-of-url-at-point
   :filename bounds-of-filename-at-point
   :line bounds-of-line-at-point
   :email bounds-of-email-at-point})

(defn bounds-of-thing-at-point
  "Find boundaries of THING-TYPE at POINT in BUFFER-STATE.

  Args:
  - buffer-state: Buffer state map
  - point: Point position
  - thing-type: Thing type keyword (:symbol, :url, etc.)

  Returns: {:start N :end M} or nil"
  [buffer-state point thing-type]
  (when-let [bounds-fn (get thing-bounds-functions thing-type)]
    (bounds-fn buffer-state point)))

(defn thing-at-point
  "Get text of THING-TYPE at POINT in BUFFER-STATE.

  Returns: String or nil"
  [buffer-state point thing-type]
  (when-let [bounds (bounds-of-thing-at-point buffer-state point thing-type)]
    (let [text (get-buffer-text buffer-state)]
      (subs text (:start bounds) (:end bounds)))))

;; -- Context-Aware Thing Detection --

(defn guess-thing-at-point
  "Try to guess what kind of thing is at POINT.

  Tries in order: URL, email, filename, symbol, word.

  Returns: {:type :keyword :bounds {:start N :end M} :text 'string'} or nil"
  [buffer-state point]
  (let [types [:url :email :filename :symbol :word]]
    (first (keep (fn [type]
                  (when-let [bounds (bounds-of-thing-at-point buffer-state point type)]
                    {:type type
                     :bounds bounds
                     :text (thing-at-point buffer-state point type)}))
                types))))

;; -- Utilities --

(defn forward-thing
  "Move point forward one THING-TYPE.

  Returns new point position."
  [buffer-state point thing-type]
  (if-let [bounds (bounds-of-thing-at-point buffer-state point thing-type)]
    ;; Move to end of current thing, then find next
    (let [next-pos (:end bounds)]
      (if-let [next-bounds (bounds-of-thing-at-point buffer-state next-pos thing-type)]
        (:end next-bounds)
        next-pos))
    point))

(defn backward-thing
  "Move point backward one THING-TYPE.

  Returns new point position."
  [buffer-state point thing-type]
  (if-let [bounds (bounds-of-thing-at-point buffer-state point thing-type)]
    ;; Move to start of current thing, then find previous
    (let [prev-pos (max 0 (dec (:start bounds)))]
      (if-let [prev-bounds (bounds-of-thing-at-point buffer-state prev-pos thing-type)]
        (:start prev-bounds)
        prev-pos))
    point))

;; -- Examples --

;; Example: Find file at point and open it
(comment
  (let [filename (thing-at-point buffer-state point :filename)]
    (when filename
      (rf/dispatch [:find-file filename]))))

;; Example: Browse URL at point
(comment
  (let [url (thing-at-point buffer-state point :url)]
    (when url
      (js/window.open url "_blank"))))

;; Example: Copy symbol at point
(comment
  (let [symbol (thing-at-point buffer-state point :symbol)]
    (when symbol
      (rf/dispatch [:kill-ring/add symbol]))))

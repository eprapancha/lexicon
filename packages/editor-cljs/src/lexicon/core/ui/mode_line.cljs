(ns lexicon.core.ui.mode-line
  "Mode-line formatting and display.

  The mode-line displays buffer information at the bottom of each window.
  It uses a format string with % constructs similar to Emacs:

  % Constructs:
  - %b: Buffer name
  - %f: File name (or buffer name if no file)
  - %l: Line number
  - %c: Column number
  - %p: Position percentage (0-100%)
  - %I: Buffer size in characters
  - %*: Modified indicator (** modified, -- unmodified, %% read-only modified, %- read-only)
  - %+: Similar to %* but shows + for modified
  - %m: Major mode name
  - %[: Begin face for recursive edit
  - %]: End face for recursive edit
  - %%: Literal %

  Example format:
  \" %*%* %b %l:%c %p (%m)\"
  Results in:
  \" -- *scratch* 1:0 0% (Lisp Interaction)\""
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]
            [lexicon.core.minibuffer :as minibuffer]))

;; -- Helper Functions --

(defn- linear-pos-to-line-col
  "Convert linear position to line/column coordinates"
  [text linear-pos]
  (if (empty? text)
    {:line 0 :column 0}
    (let [lines (str/split text #"\n" -1)  ; -1 keeps trailing empty strings
          lines-with-lengths (map count lines)]
      (loop [pos 0
             line 0
             remaining-lengths lines-with-lengths]
        (if (empty? remaining-lengths)
          {:line (max 0 (dec line)) :column 0}
          (let [line-len (first remaining-lengths)
                line-end-pos (+ pos line-len)]
            (if (<= linear-pos line-end-pos)
              {:line line :column (- linear-pos pos)}
              (recur (+ line-end-pos 1) ; +1 for newline
                     (inc line)
                     (rest remaining-lengths)))))))))

(defn- format-position-percentage
  "Calculate position percentage (0-100%) in buffer."
  [cursor buffer-size]
  (if (zero? buffer-size)
    0
    (int (* 100 (/ cursor buffer-size)))))

(defn- format-modified-indicator
  "Format the modified/read-only indicator.
  Returns:
  - '**' if modified
  - '--' if unmodified
  - '%%' if read-only and modified
  - '%-' if read-only"
  [modified? read-only?]
  (cond
    (and read-only? modified?) "%%"
    read-only?                 "%-"
    modified?                  "**"
    :else                      "--"))

(defn- format-plus-indicator
  "Format the + modified indicator.
  Returns:
  - '++' if modified
  - '--' if unmodified
  - '+%' if read-only and modified
  - '-%' if read-only"
  [modified? read-only?]
  (cond
    (and read-only? modified?) "+%"
    read-only?                 "-%"
    modified?                  "++"
    :else                      "--"))

(defn get-mode-name
  "Get the display name for a major mode.

  In Emacs, each major mode has a `mode-name` variable that provides
  the human-readable name shown in the mode-line. This function provides
  the equivalent mapping.

  Args:
    major-mode - Keyword like :clojure-mode, :fundamental-mode
    mode-data  - Optional map that may contain :mode-line-name override

  Returns:
    Human-readable mode name string (e.g., 'Clojure', 'Fundamental')"
  [major-mode mode-data]
  (or (:mode-line-name mode-data)
      (case major-mode
        ;; Core modes
        :fundamental-mode "Fundamental"
        :lisp-interaction-mode "Lisp Interaction"
        :emacs-lisp-mode "Emacs-Lisp"
        :special-mode "Special"
        :help-mode "Help"
        :buffer-menu-mode "Buffer Menu"
        :text-mode "Text"

        ;; Programming modes (Issue #130)
        :clojure-mode "Clojure"
        :javascript-mode "JavaScript"
        :python-mode "Python"
        :rust-mode "Rust"
        :html-mode "HTML"
        :css-mode "CSS"
        :markdown-mode "Markdown"

        ;; File management modes
        :dired-mode "Dired"
        :occur-mode "Occur"
        :completion-list-mode "Completions"

        ;; Outline mode
        :outline-mode "Outline"

        ;; Default: strip -mode suffix and capitalize
        (let [mode-str (name major-mode)]
          (if (clojure.string/ends-with? mode-str "-mode")
            (-> mode-str
                (clojure.string/replace #"-mode$" "")
                (clojure.string/replace #"-" " ")
                (clojure.string/split #" ")
                (->> (map clojure.string/capitalize)
                     (clojure.string/join " ")))
            (clojure.string/capitalize mode-str))))))

;; -- Format String Processor --

(defn process-mode-line-construct
  "Process a single % construct and return its string value.

  Args:
  - construct: The character after % (e.g., 'b' for %b)
  - context: Map with buffer state {:buffer-name, :line, :column, :cursor, :buffer-size, :modified?, :read-only?, :major-mode, :mode-data, :file-name, :minibuffer-depth}

  Returns: String to display for this construct"
  [construct context]
  (case construct
    \b (:buffer-name context "")
    \f (or (:file-name context) (:buffer-name context) "")
    \l (str (:line context 1))
    \c (str (:column context 0))
    \p (str (format-position-percentage (:cursor context 0) (:buffer-size context 0)) "%")
    \I (str (:buffer-size context 0))
    \* (format-modified-indicator (:modified? context false) (:read-only? context false))
    \+ (format-plus-indicator (:modified? context false) (:read-only? context false))
    \m (get-mode-name (:major-mode context :fundamental-mode) (:mode-data context {}))
    \[ (let [depth (:minibuffer-depth context 0)]
         (if (> depth 1) (str "[" depth "]") ""))  ; Show [2], [3], etc. for depth > 1
    \] ""  ; End marker for recursive edit (paired with %[)
    \% "%"
    ;; Unknown construct - return as-is
    (str "%" construct)))

(defn format-mode-line
  "Process a mode-line format string and return the formatted result.

  Args:
  - format-string: String with % constructs (e.g., \" %b %l:%c\")
  - context: Map with buffer state

  Returns: Formatted string"
  [format-string context]
  (loop [input format-string
         result ""]
    (if (empty? input)
      result
      (let [ch (first input)]
        (if (= ch \%)
          ;; Found %, process the next character as a construct
          (if (>= (count input) 2)
            (let [construct (second input)
                  replacement (process-mode-line-construct construct context)]
              (recur (subs input 2) (str result replacement)))
            ;; Lone % at end of string
            (str result "%"))
          ;; Regular character
          (recur (rest input) (str result ch)))))))

;; -- Re-frame Integration --

;; Get the formatted mode-line for a buffer
(rf/reg-sub
  :mode-line/format
  (fn [db [_ buffer-id]]
    (let [buffer (get-in db [:buffers buffer-id])
          active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
          is-active-buffer? (= buffer-id (:buffer-id active-window))

          ;; Get buffer state
          buffer-name (:name buffer "")
          file-name (:file-name buffer)
          modified? (:modified? buffer false)
          read-only? (:is-read-only? buffer false)
          major-mode (:major-mode buffer :fundamental-mode)
          mode-data (:mode-data buffer {})

          ;; Get cursor position (only accurate for active buffer)
          cursor (if is-active-buffer?
                   (get-in db [:ui :cursor-position] 0)
                   0)

          ;; Get buffer size
          wasm-instance (:wasm-instance buffer)
          text (when wasm-instance (.getText ^js wasm-instance))
          buffer-size (if text (count text) 0)

          ;; Calculate line/column
          line-col (when text
                     (linear-pos-to-line-col text cursor))
          line (if line-col (:line line-col) 1)
          column (if line-col (:column line-col) 0)

          ;; Get format string (from buffer or use default)
          ;; %[ shows minibuffer depth when > 1 (Phase 6.5 Week 3-4)
          format-string (or (:mode-line-format buffer)
                           " %[%*%* %b   %l:%c  %p  (%m)")

          ;; Get minibuffer depth (Phase 6.5 Week 3-4)
          minibuffer-depth (minibuffer/minibuffer-depth db)

          ;; Build context map
          context {:buffer-name buffer-name
                   :file-name file-name
                   :line line
                   :column column
                   :cursor cursor
                   :buffer-size buffer-size
                   :modified? modified?
                   :read-only? read-only?
                   :major-mode major-mode
                   :mode-data mode-data
                   :minibuffer-depth minibuffer-depth}]

      (format-mode-line format-string context))))

;; Set custom mode-line format for a buffer
(rf/reg-event-db
  :mode-line/set-format
  (fn [db [_ buffer-id format-string]]
    (assoc-in db [:buffers buffer-id :mode-line-format] format-string)))

;; -- Standard Mode-line Variables --

(def standard-mode-line-format
  "Standard mode-line format string used by most buffers.
  %[ shows minibuffer depth indicator when depth > 1 (Phase 6.5 Week 3-4)."
  " %[%*%* %b   %l:%c  %p  (%m)")

(def special-mode-line-format
  "Mode-line format for special (read-only) buffers."
  " %- %b   %l:%c  %p  (%m)")

(def help-mode-line-format
  "Mode-line format for help buffers."
  " %- %b  (%m)")

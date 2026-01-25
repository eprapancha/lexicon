(ns lexicon.core.api.buffer
  "Emacs-compatible buffer API functions.

   Provides point-based API for Elisp compatibility:
   - point, point-min, point-max (linear positions)
   - goto-char (move cursor to position)
   - buffer-substring (extract text range)
   - line-beginning-position, line-end-position
   - get-buffer (lookup by name)

   These functions bridge Lexicon's line/column coordinate system
   with Emacs' linear position system."
  (:require [lexicon.core.db :as db]))

;; -- Position Conversion Helpers --

(defn- split-lines
  "Split text into lines, preserving empty lines.
   Returns vector of line strings (without newlines)."
  [text]
  (if (empty? text)
    [""]
    (clojure.string/split text #"\n" -1)))

(defn line-col-to-point
  "Convert {:line N :column M} to linear byte position.
   Uses buffer's WASM instance to calculate position."
  [buffer line col]
  (when-let [^js wasm (:wasm-instance buffer)]
    (try
      (let [text (.getText wasm)
            lines (split-lines text)
            ;; Sum of all line lengths before target line + newlines
            chars-before (reduce (fn [acc line-num]
                                   (if (< line-num (count lines))
                                     (+ acc (count (nth lines line-num)) 1) ; +1 for newline
                                     acc))
                                 0
                                 (range 0 line))]
        (+ chars-before col))
      (catch js/Error e
        (js/console.error "Error converting line/col to point:" e)
        0))))

(defn point-to-line-col
  "Convert linear byte position to {:line N :column M}.
   Uses buffer's WASM instance to calculate position."
  [buffer point]
  (when-let [^js wasm (:wasm-instance buffer)]
    (try
      (let [text (.getText wasm)
            lines (split-lines text)
            total-lines (count lines)]
        (loop [line 0
               chars-seen 0]
          (if (>= line total-lines)
            {:line (max 0 (dec total-lines)) :column 0}
            (let [line-text (nth lines line)
                  line-length (count line-text)
                  chars-after-line (+ chars-seen line-length 1)] ; +1 for newline
              (if (< point chars-after-line)
                {:line line :column (- point chars-seen)}
                (recur (inc line) chars-after-line))))))
      (catch js/Error e
        (js/console.error "Error converting point to line/col:" e)
        {:line 0 :column 0}))))

;; -- Point API (Emacs-compatible) --

(defn point
  "Return the current cursor position as a linear byte offset.
   This is the primary coordinate system used by Elisp.

   Usage: (point db)
   Returns: Integer position (0-indexed)"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        cursor-pos (:cursor-position active-window)]
    (line-col-to-point active-buffer (:line cursor-pos) (:column cursor-pos))))

(defn point-min
  "Return the minimum accessible buffer position.

   With narrowing active, returns BEGV (start of narrowed region).
   Without narrowing, returns 0.

   Usage: (point-min db)
   Returns: Integer position"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        begv (get-in db [:buffers active-buffer-id :begv])]
    (or begv 0)))

(defn point-max
  "Return the maximum accessible buffer position.

   With narrowing active, returns ZV (end of narrowed region).
   Without narrowing, returns buffer length.

   Usage: (point-max db)
   Returns: Integer position (end of accessible region)"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        zv (get-in db [:buffers active-buffer-id :zv])]
    (if zv
      zv
      (when-let [^js wasm (:wasm-instance active-buffer)]
        ;; Use getText and count since getLength may not exist on all WASM instances
        (count (.getText wasm))))))

(defn goto-char
  "Move cursor to linear position POS.
   Returns event vector to dispatch.

   Usage: (goto-char db pos)
   Returns: [:cursor/set-position {:line N :column M}]"
  [db pos]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        line-col (point-to-line-col active-buffer pos)]
    [:cursor/set-position line-col]))

;; -- Buffer Content Access --

(defn buffer-string
  "Return the accessible buffer contents as a string.

   With narrowing active, returns only the narrowed region (BEGV to ZV).
   Without narrowing, returns the entire buffer.

   Usage: (buffer-string db)
   Returns: String"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        begv (get-in db [:buffers active-buffer-id :begv])
        zv (get-in db [:buffers active-buffer-id :zv])]
    (when-let [^js wasm (:wasm-instance active-buffer)]
      (let [full-text (.getText wasm)]
        (if (or begv zv)
          ;; Narrowing active - return narrowed region
          (subs full-text (or begv 0) (or zv (count full-text)))
          ;; No narrowing - return full buffer
          full-text)))))

(defn buffer-substring
  "Return text between START and END positions (linear offsets).

   Usage: (buffer-substring db start end)
   Returns: String"
  [db start end]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (when-let [^js wasm (:wasm-instance active-buffer)]
      (let [text (.getText wasm)
            actual-start (max 0 (min start end))
            actual-end (min (count text) (max start end))]
        (subs text actual-start actual-end)))))

;; -- Line Position Functions --

(defn line-beginning-position
  "Return the position at the beginning of the current line.

   Usage: (line-beginning-position db)
   Returns: Integer position"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        cursor-pos (:cursor-position active-window)
        line (:line cursor-pos)]
    (line-col-to-point active-buffer line 0)))

(defn line-end-position
  "Return the position at the end of the current line.

   Usage: (line-end-position db)
   Returns: Integer position"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        cursor-pos (:cursor-position active-window)
        line (:line cursor-pos)]
    (when-let [^js wasm (:wasm-instance active-buffer)]
      (let [text (.getText wasm)
            lines (split-lines text)
            line-text (if (< line (count lines)) (nth lines line) "")]
        (line-col-to-point active-buffer line (count line-text))))))

;; -- Buffer Lookup --

(defn get-buffer
  "Get buffer by name.
   Returns buffer map or nil if not found.

   Usage: (get-buffer db \"*scratch*\")
   Returns: Buffer map or nil"
  [db buffer-name]
  (let [buffers (:buffers db)]
    (first (filter #(= (:name %) buffer-name) (vals buffers)))))

(defn buffer-name
  "Return the name of the current buffer.

   Usage: (buffer-name db)
   Returns: String"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (:name active-buffer)))

(defn buffer-size
  "Return the size of the current buffer in characters.

   Usage: (buffer-size db)
   Returns: Integer"
  [db]
  (point-max db))

;; -- Buffer-Local Variables --

(defn buffer-local-value
  "Get the value of a buffer-local variable.

   Usage: (buffer-local-value db var-keyword)
   Returns: Value or nil"
  [db var-keyword]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (get-in active-buffer [:buffer-local-vars var-keyword])))

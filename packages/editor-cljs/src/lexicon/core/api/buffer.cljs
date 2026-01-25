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

(defn line-col-to-point
  "Convert {:line N :column M} to linear byte position.
   Uses buffer's WASM instance to calculate position."
  [buffer line col]
  (when-let [^js wasm (:wasm-instance buffer)]
    (try
      ;; Sum of all line lengths before target line + column
      ;; WASM getLineText returns text for each line
      (let [lines-before (range 0 line)
            chars-before (reduce (fn [acc line-num]
                                   (let [line-text (.getLineText wasm line-num)]
                                     (+ acc (count line-text) 1))) ; +1 for newline
                                 0
                                 lines-before)]
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
      (let [total-lines (.getLineCount wasm)]
        (loop [line 0
               chars-seen 0]
          (if (>= line total-lines)
            {:line (dec total-lines) :column 0}
            (let [line-text (.getLineText wasm line)
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
  "Return the minimum valid buffer position.
   In Emacs this is always 1, but we use 0-indexed positions.

   Usage: (point-min)
   Returns: 0"
  []
  0)

(defn point-max
  "Return the maximum valid buffer position (buffer length).

   Usage: (point-max db)
   Returns: Integer position (end of buffer)"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (when-let [^js wasm (:wasm-instance active-buffer)]
      (.getLength wasm))))

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
  "Return the entire buffer contents as a string.

   Usage: (buffer-string db)
   Returns: String"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (when-let [^js wasm (:wasm-instance active-buffer)]
      (.getText wasm))))

(defn buffer-substring
  "Return text between START and END positions (linear offsets).

   Usage: (buffer-substring db start end)
   Returns: String"
  [db start end]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (when-let [^js wasm (:wasm-instance active-buffer)]
      (let [start-pos (point-to-line-col active-buffer start)
            end-pos (point-to-line-col active-buffer end)
            start-line (:line start-pos)
            end-line (:line end-pos)]
        (if (= start-line end-line)
          ;; Same line - extract substring
          (let [line-text (.getLineText wasm start-line)]
            (subs line-text (:column start-pos) (:column end-pos)))
          ;; Multiple lines - concatenate
          (let [lines (range start-line (inc end-line))
                parts (map (fn [line-num]
                            (cond
                              (= line-num start-line)
                              (subs (.getLineText wasm line-num) (:column start-pos))

                              (= line-num end-line)
                              (subs (.getLineText wasm line-num) 0 (:column end-pos))

                              :else
                              (.getLineText wasm line-num)))
                          lines)]
            (clojure.string/join "\n" parts)))))))

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
      (let [line-text (.getLineText wasm line)]
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

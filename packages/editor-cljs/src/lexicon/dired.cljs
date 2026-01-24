(ns lexicon.dired
  "Dired (Directory Editor) - File manager for Lexicon

  Implements Emacs Dired functionality:
  - Directory listing as editable buffers
  - File marking system
  - File operations (copy, delete, rename)
  - Navigation and sorting

  Related: Issue #93"
  (:require [clojure.string :as str]))

;;; =============================================================================
;;; Dired Buffer State
;;; =============================================================================

(defn dired-buffer?
  "Check if a buffer is a Dired buffer"
  [buffer]
  (= (:major-mode buffer) :dired-mode))

(defn get-dired-state
  "Get Dired-specific state from buffer local vars"
  [buffer]
  (get-in buffer [:local-vars :dired-state]))

(defn set-dired-state
  "Set Dired-specific state in buffer local vars"
  [buffer state]
  (assoc-in buffer [:local-vars :dired-state] state))

(defn create-dired-state
  "Create initial Dired state for a directory"
  [directory entries]
  {:directory directory
   :entries entries        ; Vector of file entry maps
   :marks #{}             ; Set of marked entry indices
   :deletions #{}         ; Set of entries marked for deletion
   :sort-by :name         ; :name, :size, :modified
   :sort-reverse? false
   :point-entry nil})     ; Entry name at point (for restoration)

;;; =============================================================================
;;; File Entry Operations
;;; =============================================================================

(defn format-entry
  "Format a file entry for display in Dired buffer

  Entry format:
  {:name \"file.txt\"
   :type :file        ; or :directory
   :size 1234
   :modified \"2025-01-20T10:30:00\"
   :permissions \"rw-r--r--\"}

  Display format (simplified):
  -rw-r--r--  1234  Jan 20 10:30  file.txt
   drwxr-xr-x   512  Jan 20 09:15  subdir/"
  [{:keys [name type size modified permissions]} mark-char]
  (let [type-char (if (= type :directory) "d" "-")
        perms (or permissions "rw-r--r--")
        size-str (str size)
        mark (or mark-char " ")]
    (str mark " " type-char perms "  " (format "%6s" size-str) "  " name)))

(defn parse-dired-line
  "Parse a Dired buffer line to extract entry info

  Returns {:mark char :name string :line-number int}
  Returns nil for non-entry lines (header, empty, etc.)"
  [line line-number]
  (when-let [[_ mark perms size name]
             (re-matches #"^([* D ]) ([d-][rwx-]+)\s+(\d+)\s+(.+)$" line)]
    {:mark (first mark)
     :name (str/trim name)
     :line-number line-number}))

(defn generate-buffer-content
  "Generate the full Dired buffer content from directory state"
  [dired-state]
  (let [{:keys [directory entries marks deletions]} dired-state
        header (str "  " directory ":\n")
        entry-lines (map-indexed
                     (fn [idx entry]
                       (let [mark-char (cond
                                        (contains? deletions idx) \D
                                        (contains? marks idx) \*
                                        :else \space)]
                         (format-entry entry mark-char)))
                     entries)]
    (str header (str/join "\n" entry-lines) "\n")))

;;; =============================================================================
;;; Sorting
;;; =============================================================================

(defn sort-entries
  "Sort file entries by specified criterion"
  [entries sort-by reverse?]
  (let [sorted (case sort-by
                 :name (sort-by :name entries)
                 :size (sort-by :size entries)
                 :modified (sort-by :modified entries)
                 entries)]
    (if reverse?
      (reverse sorted)
      sorted)))

;;; =============================================================================
;;; Entry Selection
;;; =============================================================================

(defn find-entry-at-point
  "Find the entry index at the current point position

  Returns {:index int :entry map} or nil if point not on entry line"
  [buffer-text point]
  (let [lines (str/split-lines buffer-text)
        ;; Find which line point is on
        line-starts (reductions + 0 (map #(+ (count %) 1) lines))
        line-idx (count (take-while #(<= % point) line-starts))
        line (get lines line-idx)]
    (when-let [parsed (parse-dired-line line line-idx)]
      {:index (- line-idx 1)  ; Subtract header line
       :name (:name parsed)})))

;;; =============================================================================
;;; Mark Operations
;;; =============================================================================

(defn toggle-mark
  "Toggle mark on entry at index"
  [dired-state index]
  (update dired-state :marks
          (fn [marks]
            (if (contains? marks index)
              (disj marks index)
              (conj marks index)))))

(defn toggle-deletion
  "Toggle deletion flag on entry at index"
  [dired-state index]
  (update dired-state :deletions
          (fn [deletions]
            (if (contains? deletions index)
              (disj deletions index)
              (conj deletions index)))))

(defn clear-all-marks
  "Clear all marks and deletion flags"
  [dired-state]
  (assoc dired-state
         :marks #{}
         :deletions #{}))

;;; =============================================================================
;;; Filesystem Operations (Stub - to be implemented with FS provider)
;;; =============================================================================

(defn list-directory
  "List directory contents

  For now, returns mock data. Will be replaced with real FS provider.

  Returns vector of entry maps:
  [{:name \"file.txt\" :type :file :size 1234 :modified \"...\" :permissions \"...\"}
   {:name \"subdir\" :type :directory :size 512 :modified \"...\" :permissions \"...\"}]"
  [directory]
  ;; TODO: Implement with File System Access API or mock for tests
  ;; For now, return empty list
  [])

(defn delete-files
  "Delete files from filesystem

  Returns {:success [names...] :failed [{:name ... :error ...}]}"
  [directory file-names]
  ;; TODO: Implement with FS provider
  {:success []
   :failed (map #(hash-map :name % :error "Not implemented") file-names)})

(defn copy-file
  "Copy file from source to destination"
  [source destination]
  ;; TODO: Implement
  {:success false :error "Not implemented"})

(defn rename-file
  "Rename/move file"
  [old-name new-name]
  ;; TODO: Implement
  {:success false :error "Not implemented"})

(ns lexicon.core.completion.file
  "File Completion Table - Completion for file paths.

  Used by:
    - C-x C-f (find-file)
    - C-x C-s (save-buffer) for new files
    - C-x C-w (write-file)
    - M-x dired

  Features:
    - Completes from File System Access API granted directories
    - Annotations show (dir) for directories, file size for files
    - Dynamic - fetches directory contents as user types
    - Falls back to mock filesystem when FS Access unavailable

  See Issue #135, #137."
  (:require [lexicon.core.completion.table :as table]))

(defn file-annotation
  "Generate annotation for file completion.
   Shows (dir) for directories, file size for files."
  [file-info]
  (cond
    (:directory? file-info) " (dir)"
    (:size file-info) (str " (" (:size file-info) ")")
    :else ""))

(defn parse-path
  "Parse a file path into directory and filename components.
   Returns {:directory 'dir' :filename 'file'}"
  [path]
  (let [last-slash (clojure.string/last-index-of path "/")]
    (if (and last-slash (>= last-slash 0))
      {:directory (subs path 0 (inc last-slash))
       :filename (subs path (inc last-slash))}
      {:directory ""
       :filename path})))

(defn join-path
  "Join directory and filename into a path."
  [directory filename]
  (if (clojure.string/blank? directory)
    filename
    (str directory filename)))

(defn make-file-table
  "Create a dynamic completion table for file paths.

  Takes a function that returns directory contents:
    list-directory-fn: (fn [directory-path]) -> [{:name :directory? :size}]

  The table handles:
    - Parsing input into directory + partial filename
    - Completing filenames within the directory
    - Adding / suffix for directories

  Example:
    (make-file-table
      (fn [dir] (list-granted-directory dir)))"
  [list-directory-fn]
  (table/make-dynamic-table
   (fn [input]
     (let [{:keys [directory filename]} (parse-path input)
           entries (list-directory-fn directory)]
       (->> entries
            (map (fn [{:keys [name directory?]}]
                   (join-path directory
                              (if directory?
                                (str name "/")
                                name))))
            (filter #(or (clojure.string/blank? filename)
                         (clojure.string/starts-with?
                          (last (clojure.string/split % #"/"))
                          filename))))))
   {:category :file}))

(defn make-mock-file-table
  "Create a file completion table using the mock filesystem.
   Used when File System Access API is not available.

  Takes the mock-filesystem map from lisp.cljs"
  [mock-fs]
  (table/make-dynamic-table
   (fn [input]
     (let [{:keys [directory]} (parse-path input)
           dir-key (if (clojure.string/blank? directory) "/" directory)
           entries (get mock-fs dir-key [])]
       (map (fn [entry]
              (if (clojure.string/ends-with? entry "/")
                (str directory entry)
                (str directory entry)))
            entries)))
   {:category :file}))

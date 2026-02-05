(ns lexicon.core.uniquify
  "Buffer name disambiguation using directory path components.

  Implements Emacs uniquify.el functionality. When multiple buffers
  visit files with the same base name (e.g., two 'config.cljs' files),
  their buffer names are disambiguated using path components.

  Styles (uniquify-buffer-name-style):
  - :forward          - dir/file.txt, other-dir/file.txt
  - :reverse          - file.txt\\dir, file.txt\\other-dir
  - :post-forward     - file.txt|dir, file.txt|other-dir
  - :post-forward-angle-brackets - file.txt<dir>, file.txt<other-dir> (default)
  - nil               - No uniquification (old behavior)

  Based on Emacs uniquify.el (~522 lines)"
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]))

;; =============================================================================
;; Uniquify Style Configuration
;; =============================================================================

(def default-style :post-forward-angle-brackets)

(defn get-uniquify-style
  "Get current uniquify-buffer-name-style from db."
  [db]
  (get-in db [:global-vars :uniquify-buffer-name-style] default-style))

;; =============================================================================
;; Path Utilities
;; =============================================================================

(defn- basename
  "Get the filename from a path (last component)."
  [path]
  (when path
    (let [parts (str/split path #"/")]
      (last (filter seq parts)))))

(defn- directory-components
  "Get directory components from a path (excluding filename).
   Returns them in order from immediate parent to root.
   /a/b/c/file.txt -> ['c' 'b' 'a']"
  [path]
  (when path
    (let [parts (filter seq (str/split path #"/"))]
      (reverse (butlast parts)))))

(defn- format-uniquified-name
  "Format a buffer name with disambiguation components.

   style - one of :forward, :reverse, :post-forward, :post-forward-angle-brackets
   base-name - the file name (e.g., 'config.cljs')
   dir-components - list of directory components needed for disambiguation"
  [style base-name dir-components]
  (case style
    :forward
    (str (str/join "/" (reverse dir-components)) "/" base-name)

    :reverse
    (str base-name "\\" (str/join "\\" dir-components))

    :post-forward
    (str base-name "|" (str/join "/" (reverse dir-components)))

    :post-forward-angle-brackets
    (str base-name "<" (str/join "/" (reverse dir-components)) ">")

    ;; nil or unknown - return base name unchanged
    base-name))

;; =============================================================================
;; Uniquify Algorithm
;; =============================================================================

(defn- find-buffers-with-same-basename
  "Find all file-visiting buffers that have the same base filename.
   Returns a map of {buffer-id {:file-path path :name current-name}}"
  [db target-basename]
  (into {}
        (keep (fn [[id buffer]]
                (let [file-path (:file-path buffer)]
                  (when (and file-path
                             (= (basename file-path) target-basename))
                    [id {:file-path file-path
                         :name (:name buffer)}])))
              (:buffers db))))

(defn- calculate-needed-components
  "Calculate minimum directory components needed to disambiguate paths.

   Takes a list of paths that all have the same basename.
   Returns a map {path -> [list of dir components needed]}

   Algorithm: Add components one at a time until all paths are unique."
  [paths]
  (if (<= (count paths) 1)
    ;; Only one path, no disambiguation needed
    (into {} (map (fn [p] [p []]) paths))
    ;; Need to disambiguate
    (let [path->components (into {} (map (fn [p] [p (directory-components p)]) paths))]
      (loop [depth 1
             result (into {} (map (fn [p] [p []]) paths))]
        (let [;; Get current distinguished names for each path
              current-names (into {}
                                  (map (fn [[p _]]
                                         [p (take depth (get path->components p))])
                                       result))
              ;; Group paths by their current name
              name->paths (group-by (fn [p] (vec (get current-names p))) paths)
              ;; Check if all are unique
              all-unique? (every? #(= 1 (count (val %))) name->paths)]
          (if all-unique?
            ;; Done - convert to result format
            current-names
            ;; Need more depth
            (let [max-depth (apply max (map #(count (val %)) path->components))]
              (if (>= depth max-depth)
                ;; Can't disambiguate further - return what we have
                current-names
                (recur (inc depth) current-names)))))))))

(defn- apply-uniquify-renames
  "Apply buffer name changes to db.

   renames is a map of {buffer-id -> new-name}"
  [db renames]
  (reduce (fn [d [buffer-id new-name]]
            (assoc-in d [:buffers buffer-id :name] new-name))
          db
          renames))

(defn uniquify-buffer-names
  "Main uniquify function. Given a db and a file path about to be opened,
   returns updated db with all affected buffer names uniquified.

   This should be called AFTER the new buffer is created in the db."
  [db new-file-path]
  (let [style (get-uniquify-style db)]
    (if (nil? style)
      ;; Uniquify disabled
      db
      (let [target-basename (basename new-file-path)
            ;; Find all buffers with same basename (including the new one)
            matching-buffers (find-buffers-with-same-basename db target-basename)]
        (if (<= (count matching-buffers) 1)
          ;; No collision - buffer can use simple name
          db
          ;; Need to uniquify
          (let [paths (map :file-path (vals matching-buffers))
                path->components (calculate-needed-components paths)
                renames (into {}
                              (map (fn [[buffer-id {:keys [file-path]}]]
                                     (let [components (get path->components file-path [])
                                           new-name (if (empty? components)
                                                      target-basename
                                                      (format-uniquified-name style target-basename components))]
                                       [buffer-id new-name]))
                                   matching-buffers))]
            (apply-uniquify-renames db renames)))))))

(defn generate-buffer-name-for-file
  "Generate an appropriate buffer name for a new file.

   If uniquify is enabled and there's a name collision, returns the
   uniquified name. Otherwise returns the base filename."
  [db file-path]
  (let [style (get-uniquify-style db)
        base-name (basename file-path)]
    (if (nil? style)
      ;; Uniquify disabled - use base name
      base-name
      ;; Check for collision
      (let [existing-buffers (find-buffers-with-same-basename db base-name)]
        (if (empty? existing-buffers)
          ;; No collision - use base name
          base-name
          ;; Will have collision - compute what the name should be
          (let [all-paths (cons file-path (map :file-path (vals existing-buffers)))
                path->components (calculate-needed-components all-paths)
                components (get path->components file-path [])]
            (if (empty? components)
              base-name
              (format-uniquified-name style base-name components))))))))

;; =============================================================================
;; Re-frame Integration
;; =============================================================================

(rf/reg-event-db
 :uniquify/rationalize-buffer-names
 (fn [db [_ file-path]]
   "Re-uniquify all buffer names after a new file is opened.
    Called after buffer creation to rename any conflicting buffers."
   (uniquify-buffer-names db file-path)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize uniquify module with default style."
  []
  (swap! rfdb/app-db assoc-in [:global-vars :uniquify-buffer-name-style] default-style))

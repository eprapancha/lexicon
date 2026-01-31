(ns lexicon.core.completion.file-table
  "File path completion table for minibuffer file operations.

  Provides Tab completion for file paths within granted directories.
  Integrates with File System Access API.

  Completion behavior:
  - '/' or '~/' triggers directory listing
  - Partial path shows matching entries from granted directories
  - Directories shown with trailing '/'
  - Files shown with size annotation"
  (:require [clojure.string :as str]
            [lexicon.core.completion.tables :as tables]
            [lexicon.core.completion.metadata :as metadata]))

;; =============================================================================
;; Path Parsing
;; =============================================================================

(defn parse-path
  "Parse a path into components.
   Returns {:directory dir :filename name :is-dir? bool}"
  [path]
  (cond
    (str/blank? path)
    {:directory "" :filename "" :is-dir? false}

    (str/ends-with? path "/")
    {:directory path :filename "" :is-dir? true}

    :else
    (let [last-slash (str/last-index-of path "/")]
      (if last-slash
        {:directory (subs path 0 (inc last-slash))
         :filename (subs path (inc last-slash))
         :is-dir? false}
        {:directory ""
         :filename path
         :is-dir? false}))))

(defn normalize-path
  "Normalize a path (resolve . and .., remove duplicate slashes)."
  [path]
  (-> path
      (str/replace #"/+" "/")
      (str/replace #"/\./|/\.$" "/")
      ;; Handle .. by removing parent
      (str/replace #"[^/]+/\.\./" "")
      (str/replace #"[^/]+/\.\.$" "")))

;; =============================================================================
;; Directory Listing Cache
;; =============================================================================

;; Cache directory listings to avoid async calls during completion
;; Key: path, Value: {:entries [...] :timestamp ms}
(defonce directory-cache (atom {}))

(def ^:private CACHE-TTL-MS 5000)  ; 5 second cache

(defn cache-directory
  "Cache directory entries for a path."
  [path entries]
  (swap! directory-cache assoc path
         {:entries entries
          :timestamp (.now js/Date)}))

(defn get-cached-directory
  "Get cached directory entries, or nil if not cached or expired."
  [path]
  (when-let [{:keys [entries timestamp]} (get @directory-cache path)]
    (when (< (- (.now js/Date) timestamp) CACHE-TTL-MS)
      entries)))

(defn clear-directory-cache
  "Clear the directory cache."
  []
  (reset! directory-cache {}))

;; =============================================================================
;; Completion Functions
;; =============================================================================

(defn find-matching-grant
  "Find the granted directory that matches the input path.
   Returns {:grant-path P :handle H :relative-path R} or nil."
  [granted-directories input-path]
  (when (seq granted-directories)
    (first
     (keep (fn [[grant-path {:keys [handle]}]]
             (when (str/starts-with? input-path grant-path)
               {:grant-path grant-path
                :handle handle
                :relative-path (subs input-path (count grant-path))}))
           granted-directories))))

(defn list-grant-roots
  "List the root paths of all granted directories.
   Used when input is empty or just '/'."
  [granted-directories]
  (->> granted-directories
       keys
       (map #(if (str/ends-with? % "/") % (str % "/")))
       sort))

(defn format-completion
  "Format a directory entry as a completion string."
  [parent-path {:keys [name kind]}]
  (let [suffix (if (= kind "directory") "/" "")]
    (str parent-path name suffix)))

(defn filter-entries
  "Filter directory entries by prefix."
  [entries prefix]
  (if (str/blank? prefix)
    entries
    (filter #(str/starts-with? (:name %) prefix) entries)))

(defn make-file-completions
  "Generate file completions for input.
   Returns vector of completion strings."
  [input granted-directories cached-entries-fn]
  (let [path (normalize-path input)
        {:keys [directory filename]} (parse-path path)]
    (cond
      ;; Empty or just "/" - show grant roots
      (or (str/blank? path) (= path "/"))
      (list-grant-roots granted-directories)

      ;; Path starts with grant - list that directory
      (find-matching-grant granted-directories directory)
      (let [match (find-matching-grant granted-directories directory)]
        (if-let [entries (cached-entries-fn (:grant-path match))]
          (let [filtered (filter-entries entries filename)]
            (map #(format-completion directory %) filtered))
          []))

      ;; Partial match against grant roots
      :else
      (let [roots (list-grant-roots granted-directories)]
        (filter #(str/starts-with? % path) roots)))))

;; =============================================================================
;; File Completion Table
;; =============================================================================

(defn file-table
  "Create a dynamic completion table for file paths.

  Uses granted directories from fs-access state.
  Completions show:
  - Directories with trailing '/'
  - Files with size annotations (when available)

  Args:
  - db: Re-frame db (for accessing :fs-access state)

  Returns: DynamicTable for file completion"
  [db]
  (let [granted (get-in db [:fs-access :granted-directories] {})
        metadata (metadata/make-metadata
                  :category :file
                  :annotation-function :file)]
    (tables/dynamic-table
     ;; Completion function
     (fn [input _context]
       (make-file-completions
        (or input "")
        granted
        get-cached-directory))

     :metadata metadata

     :boundary-fn
     (fn [input _context]
       (tables/file-boundary input (count input)))

     :context-fn
     (fn []
       {:granted-directories granted}))))

;; =============================================================================
;; Async Directory Loading
;; =============================================================================

;; When user types a path, we may need to load directory contents
;; This is done asynchronously via effects

(defn request-directory-listing
  "Request a directory listing if not cached.
   Used to pre-populate cache for completion."
  [path granted-directories]
  (when-let [{:keys [handle]} (find-matching-grant granted-directories path)]
    (when-not (get-cached-directory path)
      ;; Return the handle so caller can dispatch the listing effect
      handle)))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn is-directory-completion?
  "Check if a completion represents a directory."
  [completion]
  (str/ends-with? completion "/"))

(defn expand-tilde
  "Expand ~ to a home directory path.
   In browser context, we use '/home' as a placeholder."
  [path]
  (if (str/starts-with? path "~/")
    (str "/home" (subs path 1))
    path))

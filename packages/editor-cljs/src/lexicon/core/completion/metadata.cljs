(ns lexicon.core.completion.metadata
  "Completion metadata system for rich completion UIs.

  Metadata provides semantic information about completion candidates:
  - Categories: What kind of thing is being completed (:command, :file, :buffer)
  - Annotations: Additional info to display (keybindings, descriptions)
  - Affixation: Prefix/suffix/annotation combined
  - Grouping: Organize candidates into groups
  - Sorting: Custom sort order

  This enables packages like Marginalia and Vertico to display rich completions."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.minibuffer :as minibuffer]))

;; -- Completion Categories --

(def completion-categories
  "Standard completion categories with default behaviors."
  {:command {:display-sort-function :alphabetical
             :annotation-function :command-annotation
             :group-function nil}
   :buffer {:display-sort-function :recent-first
            :annotation-function :buffer-annotation
            :group-function nil}
   :file {:display-sort-function :alphabetical
          :annotation-function :file-annotation
          :group-function nil}
   :variable {:display-sort-function :alphabetical
              :annotation-function :variable-annotation
              :group-function nil}
   :function {:display-sort-function :alphabetical
              :annotation-function :function-annotation
              :group-function nil}
   :face {:display-sort-function :alphabetical
          :annotation-function nil
          :group-function nil}
   :theme {:display-sort-function :alphabetical
           :annotation-function nil
           :group-function nil}
   :symbol {:display-sort-function :alphabetical
            :annotation-function nil
            :group-function nil}})

;; -- Annotation Functions --

;; Command annotation: show keybinding if available
(defn command-annotation
  "Annotate a command name with its keybinding."
  [command-name]
  (let [;; Look up keybinding for this command
        ;; For now, return placeholder - will integrate with keymap system
        keybinding nil]
    (if keybinding
      (str " (" keybinding ")")
      "")))

;; Buffer annotation: show size, mode, modified status
(defn buffer-annotation
  "Annotate a buffer name with size and mode."
  [buffer-name db]
  (let [buffer (some (fn [[id buf]]
                      (when (= (:name buf) buffer-name) buf))
                    (:buffers db))]
    (if buffer
      (let [size (if-let [^js wasm (:wasm-instance buffer)]
                  (try
                    (count (.getText ^js wasm))
                    (catch js/Error _ 0))
                  0)
            mode-name (name (:major-mode buffer :fundamental-mode))
            modified? (:modified? buffer false)]
        (str " " (when modified? "*") size " " mode-name))
      "")))

;; File annotation: show size, permissions (future)
(defn file-annotation
  "Annotate a file name with metadata."
  [file-name]
  ;; Placeholder - would need file system access
  "")

;; Variable annotation: show current value
(defn variable-annotation
  "Annotate a variable name with its current value."
  [var-name db]
  ;; Placeholder - would look up variable value in db
  "")

;; Function annotation: show argument list
(defn function-annotation
  "Annotate a function name with its signature."
  [fn-name]
  ;; Placeholder - would introspect function
  "")

;; -- Affixation Functions --

;; Affixation combines prefix, candidate, suffix, and annotation
;; Returns: [prefix candidate suffix annotation]

(defn command-affixation
  "Affix command candidates with prefix/suffix/annotation."
  [candidate]
  ["" candidate "" (command-annotation candidate)])

(defn buffer-affixation
  "Affix buffer candidates with status indicators."
  [candidate db]
  (let [buffer (some (fn [[id buf]]
                      (when (= (:name buf) candidate) buf))
                    (:buffers db))
        modified? (:modified? buffer false)
        read-only? (:is-read-only? buffer false)
        prefix (str (if modified? "*" " ")
                   (if read-only? "%" " ")
                   " ")]
    [prefix candidate "" (buffer-annotation candidate db)]))

;; -- Group Functions --

;; Group by major mode (for buffers)
(defn buffer-group-function
  "Group buffers by major mode."
  [candidate transform db]
  (if transform
    ;; Return group name for this candidate
    (let [buffer (some (fn [[id buf]]
                        (when (= (:name buf) candidate) buf))
                      (:buffers db))
          mode (:major-mode buffer :fundamental-mode)]
      (str (name mode)))
    ;; Return candidate unchanged
    candidate))

;; Group by first letter (alphabetical)
(defn alphabetical-group-function
  "Group candidates by first letter."
  [candidate transform _db]
  (if transform
    (str/upper-case (first candidate))
    candidate))

;; -- Metadata Registry --

(def annotation-functions
  "Registry of annotation functions by category."
  {:command command-annotation
   :buffer buffer-annotation
   :file file-annotation
   :variable variable-annotation
   :function function-annotation})

(def affixation-functions
  "Registry of affixation functions by category."
  {:command command-affixation
   :buffer buffer-affixation})

(def group-functions
  "Registry of group functions by category."
  {:buffer buffer-group-function
   :alphabetical alphabetical-group-function})

;; -- Metadata Application --

(defn apply-annotations
  "Apply annotation function to candidates.
  Returns candidates with annotations: [[candidate annotation] ...]"
  [candidates metadata db]
  (if-let [ann-fn-key (:annotation-function metadata)]
    (let [ann-fn (get annotation-functions ann-fn-key)]
      (if ann-fn
        (map (fn [cand]
              (let [annotation (if (= ann-fn-key :buffer)
                                (ann-fn cand db)
                                (ann-fn cand))]
                [cand annotation]))
            candidates)
        ;; No annotation function found, return candidates without annotations
        (map (fn [cand] [cand ""]) candidates)))
    ;; No annotation function specified
    (map (fn [cand] [cand ""]) candidates)))

(defn apply-affixation
  "Apply affixation function to candidates.
  Returns: [[prefix candidate suffix annotation] ...]"
  [candidates metadata db]
  (if-let [affix-fn-key (:affixation-function metadata)]
    (let [affix-fn (get affixation-functions affix-fn-key)]
      (if affix-fn
        (map (fn [cand]
              (if (= affix-fn-key :buffer)
                (affix-fn cand db)
                (affix-fn cand)))
            candidates)
        ;; No affixation function found, return default format
        (map (fn [cand] ["" cand "" ""]) candidates)))
    ;; No affixation specified, use annotation if available
    (let [annotated (apply-annotations candidates metadata db)]
      (map (fn [[cand ann]] ["" cand "" ann]) annotated))))

(defn apply-grouping
  "Apply group function to candidates.
  Returns: {group-name [candidates...] ...}"
  [candidates metadata db]
  (if-let [group-fn-key (:group-function metadata)]
    (let [group-fn (get group-functions group-fn-key)]
      (if group-fn
        (group-by (fn [cand]
                   (group-fn cand true db))
                 candidates)
        ;; No group function found, return ungrouped
        {"" candidates}))
    ;; No grouping specified
    {"" candidates}))

;; -- Metadata Construction --

(defn make-metadata
  "Create completion metadata map.

  Options:
  - :category - Completion category (:command, :buffer, :file, etc.)
  - :annotation-function - Function to annotate candidates
  - :affixation-function - Function to affix candidates (overrides annotation)
  - :group-function - Function to group candidates
  - :display-sort-function - Sort order (:alphabetical, :recent-first, custom fn)
  - :cycle-sort-function - Sort for cycling

  If :category is provided, defaults are inherited from completion-categories."
  [& {:keys [category annotation-function affixation-function
             group-function display-sort-function cycle-sort-function]
      :as opts}]
  (let [;; Get defaults from category
        category-defaults (when category
                           (get completion-categories category))
        ;; Merge explicit options with category defaults
        metadata (merge category-defaults opts)]
    metadata))

;; -- Re-frame Integration --

;; Store completion metadata in app-db during completing-read
(rf/reg-event-db
  :completion/set-metadata
  (fn [db [_ metadata]]
    (minibuffer/set-metadata db metadata)))

;; Get current completion metadata
(rf/reg-sub
  :completion/metadata
  (fn [db _]
    (minibuffer/get-completion-metadata db)))

;; -- Helper: Enrich Candidates with Metadata --

(defn enrich-candidates
  "Enrich candidates with metadata (annotations, affixation, grouping).

  Returns enriched structure suitable for display:
  {\"Group Name\" [[prefix candidate suffix annotation] ...] ...}

  If no grouping, returns single group with empty name."
  [candidates metadata db]
  (let [;; Apply affixation (includes annotations)
        affixed (apply-affixation candidates metadata db)
        ;; Extract just candidates for grouping
        cand-only (map second affixed)
        ;; Group candidates
        grouped (apply-grouping cand-only metadata db)
        ;; Rebuild affixed candidates within groups
        affixed-map (into {} (map (fn [a] [(second a) a]) affixed))
        enriched (into {}
                      (map (fn [[group-name cands]]
                            [group-name (map #(get affixed-map %) cands)])
                          grouped))]
    enriched))

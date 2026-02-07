(ns lexicon.core.completion.tables
  "Completion tables for flexible candidate sources.

  Completion tables abstract the source of completion candidates.
  Three types:
  - Static: Fixed list/set of candidates
  - Programmed: Function computes candidates on demand
  - Dynamic: Candidates change based on context/input

  Tables support:
  - Candidate retrieval (all or filtered)
  - Boundary detection (where completion starts/ends)
  - Metadata attachment (category, annotations, etc.)
  - Lazy evaluation (large candidate sets)"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.minibuffer :as minibuffer]
            [lexicon.core.completion.metadata :as metadata]
            [lexicon.core.completion.styles :as styles]))

;; -- Table Protocol --

(defprotocol CompletionTable
  "Protocol for completion tables."
  (all-completions [this]
    "Return all possible completions (may be expensive).")
  (try-completion [this input]
    "Try to complete INPUT. Returns:
     - String: unique completion
     - t: INPUT is exact match
     - nil: no completions")
  (test-completion [this candidate]
    "Test if CANDIDATE is a valid completion.")
  (get-metadata [this]
    "Return metadata map for this table.")
  (get-boundaries [this input]
    "Return [start end] boundaries for completion in INPUT.
     Default: [0 (count input)]"))

;; -- Static Table --

(defrecord StaticTable [candidates metadata]
  CompletionTable
  (all-completions [this]
    candidates)

  (try-completion [this input]
    (let [matches (filter #(str/starts-with? (str/lower-case %)
                                            (str/lower-case input))
                         candidates)]
      (cond
        (empty? matches) nil
        (= 1 (count matches)) (first matches)
        ;; Check for exact match
        (some #(= (str/lower-case %) (str/lower-case input)) matches) true
        :else (first matches))))

  (test-completion [this candidate]
    (some #(= (str/lower-case %) (str/lower-case candidate)) candidates))

  (get-metadata [this]
    metadata)

  (get-boundaries [this input]
    [0 (count input)]))

(defn static-table
  "Create a static completion table from a collection of candidates.

  Args:
  - candidates: Collection of strings
  - metadata: Optional metadata map

  Returns: StaticTable"
  [candidates & {:keys [metadata]}]
  (->StaticTable (vec candidates) metadata))

;; -- Programmed Table --

(defrecord ProgrammedTable [completion-fn metadata boundary-fn]
  CompletionTable
  (all-completions [this]
    ;; Call function with nil to get all completions
    (completion-fn nil))

  (try-completion [this input]
    (let [matches (completion-fn input)]
      (cond
        (empty? matches) nil
        (= 1 (count matches)) (first matches)
        (some #(= (str/lower-case %) (str/lower-case input)) matches) true
        :else (first matches))))

  (test-completion [this candidate]
    (let [all (completion-fn nil)]
      (some #(= (str/lower-case %) (str/lower-case candidate)) all)))

  (get-metadata [this]
    metadata)

  (get-boundaries [this input]
    (if boundary-fn
      (boundary-fn input)
      [0 (count input)])))

(defn programmed-table
  "Create a programmed completion table from a function.

  Args:
  - completion-fn: Function (input) -> [candidates...]
                   Called with nil to get all candidates
  - metadata: Optional metadata map
  - boundary-fn: Optional (input) -> [start end]

  Returns: ProgrammedTable"
  [completion-fn & {:keys [metadata boundary-fn]}]
  (->ProgrammedTable completion-fn metadata boundary-fn))

;; -- Dynamic Table --

(defrecord DynamicTable [completion-fn metadata boundary-fn context-fn]
  CompletionTable
  (all-completions [this]
    ;; Get current context and call function
    (let [context (when context-fn (context-fn))]
      (completion-fn nil context)))

  (try-completion [this input]
    (let [context (when context-fn (context-fn))
          matches (completion-fn input context)]
      (cond
        (empty? matches) nil
        (= 1 (count matches)) (first matches)
        (some #(= (str/lower-case %) (str/lower-case input)) matches) true
        :else (first matches))))

  (test-completion [this candidate]
    (let [context (when context-fn (context-fn))
          all (completion-fn nil context)]
      (some #(= (str/lower-case %) (str/lower-case candidate)) all)))

  (get-metadata [this]
    metadata)

  (get-boundaries [this input]
    (if boundary-fn
      (let [context (when context-fn (context-fn))]
        (boundary-fn input context))
      [0 (count input)])))

(defn dynamic-table
  "Create a dynamic completion table that depends on context.

  Args:
  - completion-fn: Function (input context) -> [candidates...]
  - metadata: Optional metadata map
  - boundary-fn: Optional (input context) -> [start end]
  - context-fn: Optional () -> context-map

  Returns: DynamicTable"
  [completion-fn & {:keys [metadata boundary-fn context-fn]}]
  (->DynamicTable completion-fn metadata boundary-fn context-fn))

;; -- Boundary Detection --

(defn word-boundary
  "Find word boundaries in INPUT at position POS.
  Returns [start end] where:
  - start: beginning of current word
  - end: end of current word (POS)

  Word delimiters: whitespace, punctuation (except hyphens/underscores)"
  [input pos]
  (let [text (subs input 0 pos)
        ;; Find start: search backward for delimiter
        start (loop [i (dec (count text))]
                (cond
                  (< i 0) 0
                  (re-matches #"[\s\(\)\[\]\{\},;\"']" (str (nth text i)))
                  (inc i)
                  :else
                  (recur (dec i))))
        end pos]
    [start end]))

(defn symbol-boundary
  "Find symbol boundaries (Elisp-style: alphanumeric + hyphens/underscores).
  More permissive than word-boundary."
  [input pos]
  (let [text (subs input 0 pos)
        start (loop [i (dec (count text))]
                (cond
                  (< i 0) 0
                  (re-matches #"[^a-zA-Z0-9\-_]" (str (nth text i)))
                  (inc i)
                  :else
                  (recur (dec i))))
        end pos]
    [start end]))

(defn file-boundary
  "Find file path boundaries (allows slashes, dots, etc.)."
  [input pos]
  (let [text (subs input 0 pos)
        ;; File names: alphanumeric + / . - _ ~
        start (loop [i (dec (count text))]
                (cond
                  (< i 0) 0
                  (re-matches #"[\s\(\)\[\]\{\},;\"']" (str (nth text i)))
                  (inc i)
                  :else
                  (recur (dec i))))
        end pos]
    [start end]))

;; -- Built-in Tables --

;; Command table (all interactive commands)
(defn command-table [db]
  (let [commands (keys (:commands db))
        candidates (map name commands)
        metadata (metadata/make-metadata
                  :category :command
                  :annotation-function :command)]
    (static-table candidates :metadata metadata)))

;; Buffer table (all buffers)
(defn buffer-table [db]
  (let [buffers (vals (:buffers db))
        candidates (map :name buffers)
        metadata (metadata/make-metadata
                  :category :buffer
                  :annotation-function :buffer
                  :affixation-function :buffer)]
    (static-table candidates :metadata metadata)))

;; Variable table (all variables - placeholder for now)
(defn variable-table [db]
  (let [;; Placeholder - would enumerate db keys or custom variable registry
        candidates []
        metadata (metadata/make-metadata
                  :category :variable
                  :annotation-function :variable)]
    (static-table candidates :metadata metadata)))

;; Function table (all functions - placeholder for now)
(defn function-table [db]
  (let [;; Placeholder - would enumerate registered functions
        candidates []
        metadata (metadata/make-metadata
                  :category :function
                  :annotation-function :function)]
    (static-table candidates :metadata metadata)))

;; Face table (all faces)
(defn face-table [db]
  (let [faces (keys (get-in db [:faces :definitions] {}))
        candidates (map name faces)
        metadata (metadata/make-metadata :category :face)]
    (static-table candidates :metadata metadata)))

;; Theme table (all themes)
(defn theme-table [db]
  (let [themes (keys (get-in db [:themes :available-themes] {}))
        candidates (map name themes)
        metadata (metadata/make-metadata :category :theme)]
    (static-table candidates :metadata metadata)))

;; Symbol table (for *scratch* buffer and Elisp)
(defn symbol-table [db]
  (let [;; Combine commands, faces, themes, etc.
        commands (map name (keys (:commands db)))
        faces (map name (keys (get-in db [:faces :definitions] {})))
        themes (map name (keys (get-in db [:themes :available-themes] {})))
        candidates (concat commands faces themes)
        metadata (metadata/make-metadata :category :symbol)]
    (static-table candidates :metadata metadata)))

;; -- Completion Table Utilities --

(defn complete-with-table
  "Complete INPUT using TABLE and optional STYLES.

  Args:
  - table: CompletionTable instance
  - input: User input string
  - styles: Optional styles list (uses table metadata if not provided)

  Returns: Filtered candidates"
  [table input & {:keys [styles db]}]
  (let [all-candidates (all-completions table)
        metadata (get-metadata table)
        effective-styles (or styles
                            (when metadata
                              ;; Get category-specific styles from metadata
                              (when-let [category (:category metadata)]
                                (get-in styles/default-styles-config
                                       [:category-overrides category])))
                            ;; Fall back to global default
                            (:global styles/default-styles-config))]
    (if (str/blank? input)
      all-candidates
      (styles/filter-candidates input all-candidates
                               :styles-list effective-styles))))

(defn complete-with-action
  "Higher-level completion using a table or action.

  ACTION can be:
  - CompletionTable instance
  - Vector of candidates (converted to static table)
  - Function (input) -> candidates (converted to programmed table)

  Returns: [candidates metadata]"
  [action input & {:keys [db]}]
  (let [table (cond
                ;; Already a table
                (satisfies? CompletionTable action)
                action

                ;; Vector -> static table
                (vector? action)
                (static-table action)

                ;; Function -> programmed table
                (fn? action)
                (programmed-table action)

                :else
                (throw (ex-info "Invalid completion action"
                               {:action action})))

        candidates (complete-with-table table input :db db)
        metadata (get-metadata table)]

    [candidates metadata]))

;; -- Re-frame Integration --

;; Store completion table in minibuffer state
(rf/reg-event-db
  :completion/set-table
  (fn [db [_ table]]
    (minibuffer/set-completion-table db table)))

;; Get current completion table
(rf/reg-sub
  :completion/table
  (fn [db _]
    (minibuffer/get-completion-table db)))

;; Complete using current table
(rf/reg-event-db
  :completion/complete-with-table
  (fn [db [_]]
    (let [table (minibuffer/get-completion-table db)
          input (minibuffer/get-input db)]
      (if table
        (let [candidates (complete-with-table table input :db db)]
          (minibuffer/set-filtered-completions db candidates))
        db))))

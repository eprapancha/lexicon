(ns lexicon.core.completion.table
  "Completion Table Protocol - Emacs-style completion tables.

  This implements the Emacs completion table protocol where tables are
  functions that take (string predicate action) and return different
  results based on the action:

  - action = nil      → try-completion: longest common prefix
  - action = t        → all-completions: all matching candidates
  - action = :lambda  → test-completion: true if exact match exists
  - action = :metadata → metadata about the completion table

  See Issue #137 and Emacs manual 'Programmed Completion'.

  Usage:
    (def my-table (make-completion-table candidates))
    (try-completion my-table \"foo\")     ; Returns common prefix or t/nil
    (all-completions my-table \"foo\")    ; Returns all matches
    (test-completion my-table \"foobar\") ; Returns true if exact match")

;; =============================================================================
;; Completion Table Protocol
;; =============================================================================

(defprotocol ICompletionTable
  "Protocol for Emacs-style completion tables."
  (try-completion [this string predicate]
    "Attempt completion on STRING with optional PREDICATE filter.
     Returns:
     - nil if no matches
     - t if STRING is exact and unique match
     - the longest common prefix if multiple matches
     - STRING if it's an exact match with more possible")

  (all-completions [this string predicate]
    "Return all completions of STRING matching optional PREDICATE.
     Returns a sequence of all matching candidates.")

  (test-completion [this string predicate]
    "Test if STRING is an exact match in completion table.
     Returns true if STRING exactly matches a candidate.")

  (completion-metadata [this string]
    "Return metadata about the completion table.
     Returns a map with keys like:
     - :category - symbol identifying completion type (:buffer, :file, :command)
     - :annotation-function - fn to generate annotations
     - :display-sort-function - fn to sort display order
     - :cycle-sort-function - fn to sort cycling order"))

;; =============================================================================
;; Static Completion Table (simple list of strings)
;; =============================================================================

(defn- common-prefix
  "Find the longest common prefix of a collection of strings."
  [strings]
  (if (empty? strings)
    ""
    (let [sorted (sort strings)
          first-str (first sorted)
          last-str (last sorted)
          len (min (count first-str) (count last-str))]
      (loop [i 0]
        (if (and (< i len)
                 (= (nth first-str i) (nth last-str i)))
          (recur (inc i))
          (subs first-str 0 i))))))

(defn- matches-prefix?
  "Check if candidate starts with prefix (case-insensitive)."
  [candidate prefix]
  (clojure.string/starts-with?
   (clojure.string/lower-case candidate)
   (clojure.string/lower-case prefix)))

(defrecord StaticCompletionTable [candidates metadata-map]
  ICompletionTable

  (try-completion [_ string predicate]
    (let [pred (or predicate (constantly true))
          matches (->> candidates
                       (filter #(matches-prefix? % string))
                       (filter pred))]
      (cond
        (empty? matches) nil
        (= (count matches) 1)
        (if (= (first matches) string)
          t  ; Exact unique match
          (first matches))  ; Complete to the only match
        :else
        (let [prefix (common-prefix matches)]
          (if (= prefix string)
            string  ; Already at common prefix, more possible
            prefix)))))

  (all-completions [_ string predicate]
    (let [pred (or predicate (constantly true))]
      (->> candidates
           (filter #(matches-prefix? % string))
           (filter pred)
           vec)))

  (test-completion [_ string predicate]
    (let [pred (or predicate (constantly true))]
      (some #(and (= % string) (pred %)) candidates)))

  (completion-metadata [_ _string]
    metadata-map))

(defn make-completion-table
  "Create a static completion table from a collection of candidates.

  Options:
    :category - Symbol identifying type (:buffer, :file, :command, etc.)
    :annotation-fn - Function (candidate) -> annotation string
    :sort-fn - Function to sort candidates for display

  Example:
    (make-completion-table [\"foo\" \"bar\" \"baz\"]
                          {:category :custom
                           :annotation-fn (fn [s] (str \" (\" (count s) \" chars)\"))})"
  ([candidates]
   (make-completion-table candidates {}))
  ([candidates opts]
   (->StaticCompletionTable
    (vec candidates)
    {:category (:category opts :general)
     :annotation-function (:annotation-fn opts)
     :display-sort-function (:sort-fn opts)})))

;; =============================================================================
;; Dynamic Completion Table (lazy/computed)
;; =============================================================================

(defrecord DynamicCompletionTable [candidates-fn metadata-map]
  ICompletionTable

  (try-completion [_ string predicate]
    (let [candidates (candidates-fn string)
          pred (or predicate (constantly true))
          matches (->> candidates
                       (filter #(matches-prefix? % string))
                       (filter pred))]
      (cond
        (empty? matches) nil
        (= (count matches) 1)
        (if (= (first matches) string) t (first matches))
        :else
        (let [prefix (common-prefix matches)]
          (if (= prefix string) string prefix)))))

  (all-completions [_ string predicate]
    (let [candidates (candidates-fn string)
          pred (or predicate (constantly true))]
      (->> candidates
           (filter #(matches-prefix? % string))
           (filter pred)
           vec)))

  (test-completion [_ string predicate]
    (let [candidates (candidates-fn string)
          pred (or predicate (constantly true))]
      (some #(and (= % string) (pred %)) candidates)))

  (completion-metadata [_ _string]
    metadata-map))

(defn make-dynamic-table
  "Create a dynamic completion table with lazy candidate generation.

  candidates-fn: (fn [input-string]) -> collection of candidates
                 Called each time completions are needed.

  Example:
    (make-dynamic-table
      (fn [s] (filter #(str/starts-with? % s) (get-all-files)))
      {:category :file})"
  ([candidates-fn]
   (make-dynamic-table candidates-fn {}))
  ([candidates-fn opts]
   (->DynamicCompletionTable
    candidates-fn
    {:category (:category opts :dynamic)
     :annotation-function (:annotation-fn opts)
     :display-sort-function (:sort-fn opts)})))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn completing-read
  "Prompt for input with completion from TABLE.

  Returns a map with keys to configure minibuffer activation:
    :prompt - The prompt string
    :completions - All candidates (or initial set for dynamic)
    :completion-table - The table for advanced completion
    :on-confirm - Event to dispatch on confirmation

  Example:
    (completing-read \"Buffer: \" buffer-table :on-confirm [:switch-buffer])"
  [prompt table & {:keys [on-confirm initial-input require-match]}]
  (let [all-candidates (all-completions table "" nil)]
    {:prompt prompt
     :completions all-candidates
     :completion-table table
     :on-confirm on-confirm
     :input (or initial-input "")
     :require-match require-match}))

(ns lexicon.core.completion.styles
  "Completion styles for flexible candidate matching.

  Styles determine how user input matches completion candidates:
  - basic: Prefix matching (already exists in minibuffer)
  - substring: Match anywhere in candidate
  - flex: Flexible matching (chars in order, gaps allowed)
  - initials: Match initial letters (e.g., 'fb' matches 'forward-button')
  - partial-completion: Complete parts separately (e.g., 'f-b' matches 'forward-button')
  - orderless: Space-separated patterns (all must match)

  Styles are tried in order from completion-styles list.
  First style that produces matches wins."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; -- Orderless Configuration --

(defonce orderless-config
  (atom {:component-separator #"\s+"
         :matching-styles [:literal :regexp :flex]
         :style-dispatchers []
         :smart-case true}))

;; -- Matching Functions --

(defn basic-match?
  "Basic prefix matching. Returns true if candidate starts with pattern."
  [pattern candidate]
  (str/starts-with? (str/lower-case candidate) (str/lower-case pattern)))

(defn substring-match?
  "Substring matching. Returns true if pattern appears anywhere in candidate."
  [pattern candidate]
  (str/includes? (str/lower-case candidate) (str/lower-case pattern)))

(defn flex-match?
  "Flex matching. Pattern chars must appear in order, but gaps are allowed.
  Example: 'eec' matches 'execute-extended-command'"
  [pattern candidate]
  (let [pattern-lower (str/lower-case pattern)
        candidate-lower (str/lower-case candidate)]
    (loop [p-idx 0
           c-idx 0]
      (cond
        ;; Matched all pattern chars
        (>= p-idx (count pattern-lower))
        true

        ;; Ran out of candidate chars
        (>= c-idx (count candidate-lower))
        false

        ;; Current chars match, advance both
        (= (nth pattern-lower p-idx) (nth candidate-lower c-idx))
        (recur (inc p-idx) (inc c-idx))

        ;; Current chars don't match, advance candidate only
        :else
        (recur p-idx (inc c-idx))))))

(defn initials-match?
  "Initials matching. Pattern matches first letters of words.
  Example: 'fb' matches 'forward-button', 'swtb' matches 'switch-to-buffer'"
  [pattern candidate]
  (let [pattern-lower (str/lower-case pattern)
        ;; Extract initials: first char + chars after hyphens/underscores
        initials (->> candidate
                     (partition-by #(contains? #{\- \_ \space} %))
                     (remove #(contains? #{\- \_ \space} (first %)))
                     (map first)
                     (map str/lower-case)
                     (str/join))]
    (str/starts-with? initials pattern-lower)))

(defn partial-completion-match?
  "Partial completion matching. Complete parts separated by delimiters.
  Example: 'f-b' matches 'forward-button', 'sw-bu' matches 'switch-buffer'"
  [pattern candidate]
  (let [pattern-lower (str/lower-case pattern)
        candidate-lower (str/lower-case candidate)
        ;; Split pattern by delimiter
        pattern-parts (str/split pattern-lower #"[-_\s]")
        ;; Split candidate by delimiter
        candidate-parts (str/split candidate-lower #"[-_\s]")]
    ;; Each pattern part must match the start of corresponding candidate part
    (and (<= (count pattern-parts) (count candidate-parts))
         (every? (fn [[p-part c-part]]
                  (str/starts-with? c-part p-part))
                (map vector pattern-parts candidate-parts)))))

(defn regexp-match?
  "Regexp matching. Treats pattern as a regular expression.
  Example: 'for.*char' matches 'forward-char'"
  [pattern candidate]
  (try
    (boolean (re-find (re-pattern pattern) candidate))
    (catch :default _ false)))

;; -- Orderless Component Matching --

(defn- smart-case?
  "Returns true if pattern should use case-insensitive matching.
  All-lowercase patterns are case-insensitive; patterns with uppercase
  chars are case-sensitive."
  [pattern]
  (= pattern (str/lower-case pattern)))

(def ^:private component-style-fns
  "Map of component matching style keywords to their match functions.
  These are used by the orderless engine per-component."
  {:literal   (fn [p c] (str/includes? c p))
   :regexp    (fn [p c] (try (boolean (re-find (re-pattern p) c)) (catch :default _ false)))
   :flex      (fn [pattern candidate]
                (loop [p-idx 0 c-idx 0]
                  (cond
                    (>= p-idx (count pattern)) true
                    (>= c-idx (count candidate)) false
                    (= (nth pattern p-idx) (nth candidate c-idx))
                    (recur (inc p-idx) (inc c-idx))
                    :else (recur p-idx (inc c-idx)))))
   :prefix    (fn [p c] (str/starts-with? c p))
   :initialism (fn [pattern candidate]
                 (let [initials (->> candidate
                                     (partition-by #(contains? #{\- \_ \space} %))
                                     (remove #(contains? #{\- \_ \space} (first %)))
                                     (map first)
                                     (apply str))]
                   (str/starts-with? initials pattern)))})

(defn parse-dispatch
  "Parse affix dispatch characters from a component.
  Returns {:pattern string :style keyword-or-nil :negate? boolean}

  Dispatch characters:
  - =foo  literal match
  - ~foo  flex match
  - !foo  negation (must NOT match)
  - ^foo  prefix match
  - ,foo  initialism match"
  [component]
  (if (>= (count component) 2)
    (let [first-char (first component)
          rest-str (subs component 1)]
      (case first-char
        \= {:pattern rest-str :style :literal :negate? false}
        \~ {:pattern rest-str :style :flex :negate? false}
        \! {:pattern rest-str :style nil :negate? true}
        \^ {:pattern rest-str :style :prefix :negate? false}
        \, {:pattern rest-str :style :initialism :negate? false}
        {:pattern component :style nil :negate? false}))
    {:pattern component :style nil :negate? false}))

(defn- try-component-styles
  "Try matching a component against a candidate using the given styles.
  If dispatched-style is non-nil, use only that style.
  Otherwise try styles in order until one matches."
  [pattern candidate styles smart-case-enabled? dispatched-style]
  (let [case-p (if (and smart-case-enabled? (smart-case? pattern))
                 (str/lower-case pattern)
                 pattern)
        case-c (if (and smart-case-enabled? (smart-case? pattern))
                 (str/lower-case candidate)
                 candidate)]
    (if dispatched-style
      ;; Use only the dispatched style
      (when-let [match-fn (get component-style-fns dispatched-style)]
        (match-fn case-p case-c))
      ;; Try styles in order
      (some (fn [style-key]
              (when-let [match-fn (get component-style-fns style-key)]
                (match-fn case-p case-c)))
            styles))))

(defn orderless-match?
  "Orderless matching. Space-separated patterns, all must match (any order).
  Supports affix dispatchers (=literal, ~flex, !negation, ^prefix, ,initialism),
  configurable matching styles, and smart case.
  Example: 'buf mode' matches 'buffer-menu-mode', 'mode buf' also matches"
  [pattern candidate]
  (let [config @orderless-config
        components (remove str/blank?
                           (str/split pattern (:component-separator config)))
        styles (:matching-styles config)
        smart-case-enabled? (:smart-case config)]
    (if (empty? components)
      false
      (every? (fn [component]
                (let [{:keys [pattern style negate?]} (parse-dispatch component)
                      match? (try-component-styles
                              pattern candidate styles
                              smart-case-enabled? style)]
                  (if negate? (not match?) match?)))
              components))))

;; -- Match Highlighting --

(defn highlight-basic-match
  "Return indices of matched chars for basic (prefix) match."
  [pattern candidate]
  (when (basic-match? pattern candidate)
    (range 0 (min (count pattern) (count candidate)))))

(defn highlight-substring-match
  "Return indices of matched chars for substring match."
  [pattern candidate]
  (when-let [idx (str/index-of (str/lower-case candidate)
                               (str/lower-case pattern))]
    (range idx (+ idx (count pattern)))))

(defn highlight-flex-match
  "Return indices of matched chars for flex match."
  [pattern candidate]
  (when (flex-match? pattern candidate)
    (let [pattern-lower (str/lower-case pattern)
          candidate-lower (str/lower-case candidate)]
      (loop [p-idx 0
             c-idx 0
             matched-indices []]
        (cond
          (>= p-idx (count pattern-lower))
          matched-indices

          (>= c-idx (count candidate-lower))
          nil

          (= (nth pattern-lower p-idx) (nth candidate-lower c-idx))
          (recur (inc p-idx) (inc c-idx) (conj matched-indices c-idx))

          :else
          (recur p-idx (inc c-idx) matched-indices))))))

(defn- highlight-component
  "Return matched char indices for a single orderless component against candidate."
  [component candidate smart-case-enabled? styles]
  (let [{:keys [pattern style negate?]} (parse-dispatch component)]
    (when-not negate?
      (let [case-p (if (and smart-case-enabled? (smart-case? pattern))
                     (str/lower-case pattern)
                     pattern)
            case-c (if (and smart-case-enabled? (smart-case? pattern))
                     (str/lower-case candidate)
                     candidate)
            ;; Determine which style matched
            matched-style (or style
                              (some (fn [s]
                                      (when-let [mfn (get component-style-fns s)]
                                        (when (mfn case-p case-c) s)))
                                    styles))]
        (case matched-style
          :literal
          (when-let [idx (str/index-of case-c case-p)]
            (range idx (+ idx (count case-p))))

          :prefix
          (when (str/starts-with? case-c case-p)
            (range 0 (count case-p)))

          (:flex nil)
          (loop [p-idx 0 c-idx 0 indices []]
            (cond
              (>= p-idx (count case-p)) indices
              (>= c-idx (count case-c)) nil
              (= (nth case-p p-idx) (nth case-c c-idx))
              (recur (inc p-idx) (inc c-idx) (conj indices c-idx))
              :else (recur p-idx (inc c-idx) indices)))

          :regexp
          (when-let [m (try (re-find (re-pattern case-p) case-c) (catch :default _ nil))]
            (let [match-str (if (string? m) m (first m))]
              (when-let [idx (str/index-of case-c match-str)]
                (range idx (+ idx (count match-str))))))

          :initialism
          (let [initials-indices
                (loop [chars (seq case-p)
                       idx 0
                       at-word-start? true
                       indices []]
                  (cond
                    (empty? chars) indices
                    (>= idx (count case-c)) nil
                    (and at-word-start?
                         (= (first chars) (nth case-c idx)))
                    (recur (rest chars) (inc idx) false (conj indices idx))
                    (contains? #{\- \_ \space} (nth case-c idx))
                    (recur chars (inc idx) true indices)
                    :else
                    (recur chars (inc idx) false indices)))]
            initials-indices)

          ;; fallback
          nil)))))

(defn highlight-orderless-match
  "Return indices of matched chars for orderless match."
  [pattern candidate]
  (when (orderless-match? pattern candidate)
    (let [config @orderless-config
          components (remove str/blank?
                             (str/split pattern (:component-separator config)))
          styles (:matching-styles config)
          smart-case-enabled? (:smart-case config)
          all-indices (mapcat #(highlight-component % candidate smart-case-enabled? styles)
                              components)]
      (distinct (sort all-indices)))))

;; -- Style Definitions --

(def completion-styles
  "Available completion styles with their matching functions."
  {:basic {:match-fn basic-match?
           :highlight-fn highlight-basic-match
           :description "Prefix matching"}

   :substring {:match-fn substring-match?
               :highlight-fn highlight-substring-match
               :description "Match anywhere in candidate"}

   :flex {:match-fn flex-match?
          :highlight-fn highlight-flex-match
          :description "Flexible matching (chars in order, gaps allowed)"}

   :initials {:match-fn initials-match?
              :highlight-fn nil
              :description "Match initial letters of words"}

   :partial-completion {:match-fn partial-completion-match?
                        :highlight-fn nil
                        :description "Complete parts separated by delimiters"}

   :orderless {:match-fn orderless-match?
               :highlight-fn highlight-orderless-match
               :description "Space-separated patterns (all must match)"}})

;; -- Style Application --

(defn try-style
  "Try a single style against candidates. Returns matches or nil."
  [style-key pattern candidates]
  (when-let [style (get completion-styles style-key)]
    (let [match-fn (:match-fn style)
          matches (filter #(match-fn pattern %) candidates)]
      (when (seq matches)
        {:style style-key
         :matches matches}))))

(defn apply-styles
  "Apply styles in order until one produces matches.

  Args:
  - styles-list: Ordered list of style keywords (e.g., [:basic :substring :flex])
  - pattern: User input pattern
  - candidates: List of completion candidates

  Returns: {:style :keyword :matches [candidates...]} or nil"
  [styles-list pattern candidates]
  (when-not (str/blank? pattern)
    (some (fn [style-key]
           (try-style style-key pattern candidates))
         styles-list)))

(defn filter-candidates
  "Filter candidates using completion styles.

  Uses first style that produces matches. If no pattern, returns all candidates.

  Args:
  - pattern: User input
  - candidates: List of candidates
  - styles-list: Ordered list of styles to try (default: [:basic :substring :flex])

  Returns: Filtered candidates"
  [pattern candidates & {:keys [styles-list]
                         :or {styles-list [:basic :substring :flex]}}]
  (if (str/blank? pattern)
    candidates
    (if-let [result (apply-styles styles-list pattern candidates)]
      (:matches result)
      [])))

(defn highlight-matches
  "Return match highlighting info for a candidate.

  Args:
  - pattern: User input
  - candidate: Candidate to highlight
  - style: Style keyword used for matching

  Returns: Set of character indices to highlight, or nil"
  [pattern candidate style]
  (when-let [highlight-fn (:highlight-fn (get completion-styles style))]
    (set (highlight-fn pattern candidate))))

;; -- Re-frame Integration --

;; Set completion styles for current session
(rf/reg-event-db
  :completion/set-styles
  (fn [db [_ styles-list]]
    (assoc-in db [:completion :styles] styles-list)))

;; Get current completion styles
(rf/reg-sub
  :completion/styles
  (fn [db _]
    (get-in db [:completion :styles] [:basic :substring :flex])))

;; Set category-specific style overrides
(rf/reg-event-db
  :completion/set-category-override
  (fn [db [_ category styles-list]]
    (assoc-in db [:completion :category-overrides category] styles-list)))

;; Get style override for category
(rf/reg-sub
  :completion/category-override
  (fn [db [_ category]]
    (get-in db [:completion :category-overrides category])))

;; Get effective styles for current completion
(rf/reg-sub
  :completion/effective-styles
  :<- [:completion/styles]
  :<- [:completion/metadata]
  (fn [[default-styles metadata] _]
    (if-let [category (:category metadata)]
      ;; Check for category override
      (let [override @(rf/subscribe [:completion/category-override category])]
        (or override default-styles))
      ;; No category, use default
      default-styles)))

;; -- Style Configuration --

;; Default style configuration
(def default-styles-config
  "Default completion styles configuration."
  {:global [:basic :substring :flex]
   :category-overrides {:file [:basic :partial-completion :substring]
                        :buffer [:basic :substring]
                        :command [:basic :flex :substring :orderless]}})

;; Initialize styles system
(defn initialize-styles! []
  (rf/dispatch [:completion/set-styles (:global default-styles-config)])
  (doseq [[category styles] (:category-overrides default-styles-config)]
    (rf/dispatch [:completion/set-category-override category styles])))

;; Auto-initialize on namespace load
(initialize-styles!)

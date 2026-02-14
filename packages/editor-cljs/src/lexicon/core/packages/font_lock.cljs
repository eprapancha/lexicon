(ns lexicon.core.packages.font-lock
  "Emacs-compatible font-lock (syntax highlighting) system.

  Font-lock applies faces to buffer text based on mode-specific keywords.
  Keywords are patterns (regexes) that match syntax elements and associate
  them with faces (colors/styles).

  Architecture:
  - font-lock-keywords: Per-mode list of (MATCHER . FACE) patterns
  - font-lock-mode: Minor mode that enables highlighting for a buffer
  - Fontification: Process of applying faces to text via text properties

  Key functions:
  - font-lock-fontify-region: Apply keywords to a region
  - font-lock-fontify-buffer: Apply keywords to entire buffer
  - font-lock-mode: Toggle syntax highlighting

  Storage:
  - Face properties stored in [:buffers id :text-properties :face]
  - Mode keywords in [:font-lock :keywords mode-name]

  Based on Emacs lisp/font-lock.el

  Issue #130"
  (:require [re-frame.core :as rf]
            [lexicon.core.text-properties :as text-props]
            [lexicon.lisp :as lisp]
            [clojure.string :as str]))

;; =============================================================================
;; Font-Lock Faces (Standard Emacs Faces)
;; =============================================================================

;; These face keywords correspond to faces defined in faces.cljs
(def standard-faces
  "Standard font-lock face keywords.
   Each face has a corresponding entry in faces.cljs with colors."
  #{:font-lock-keyword-face
    :font-lock-string-face
    :font-lock-comment-face
    :font-lock-comment-delimiter-face
    :font-lock-function-name-face
    :font-lock-variable-name-face
    :font-lock-type-face
    :font-lock-constant-face
    :font-lock-builtin-face
    :font-lock-warning-face
    :font-lock-preprocessor-face
    :font-lock-number-face})

;; =============================================================================
;; Font-Lock Keywords Data Structure
;; =============================================================================

;; Keywords format: Vector of keyword specs
;; Each spec can be:
;; - [regex face] - Simple: match regex, apply face to entire match
;; - [regex group face] - Group: apply face to specific capture group
;; - [regex [[group1 face1] [group2 face2]...]] - Multiple groups
;;
;; Example for Clojure:
;; [["\\bdef\\w*\\b" :font-lock-keyword-face]
;;  ["\"[^\"]*\"" :font-lock-string-face]
;;  [";.*$" :font-lock-comment-face]]

(def clojure-font-lock-keywords
  "Font-lock keywords for Clojure/ClojureScript mode."
  [;; Comments (single-line)
   [";.*$" :font-lock-comment-face]

   ;; Strings (double-quoted, handles escapes)
   ["\"(?:[^\"\\\\]|\\\\.)*\"" :font-lock-string-face]

   ;; Keywords (:keyword, ::keyword)
   ["::?[a-zA-Z][a-zA-Z0-9*+!_'?<>=-]*" :font-lock-constant-face]

   ;; Definition forms
   ["\\(\\s*(def[a-z-]*)" 1 :font-lock-keyword-face]

   ;; Function/macro names after def forms
   ["\\(def[a-z-]*\\s+([a-zA-Z][a-zA-Z0-9*+!_'?<>=-]*)" 1 :font-lock-function-name-face]

   ;; Special forms and macros
   ["\\b(if|when|cond|let|fn|do|loop|recur|throw|try|catch|finally|quote|var|import|require|use|ns|in-ns|refer|defn|defn-|defmacro|defmethod|defmulti|defprotocol|defrecord|deftype|extend-type|extend-protocol|reify|proxy|doseq|dotimes|for|while|binding|with-open|with-local-vars|declare|case|condp|->|->>|->>|as->|cond->|some->|and|or|not)\\b" :font-lock-keyword-face]

   ;; Built-in functions
   ["\\b(nil\\?|true\\?|false\\?|some\\?|empty\\?|seq\\?|map\\?|vector\\?|set\\?|list\\?|string\\?|number\\?|keyword\\?|symbol\\?|fn\\?|first|second|rest|next|last|butlast|nth|get|get-in|assoc|assoc-in|dissoc|update|update-in|select-keys|keys|vals|merge|into|conj|cons|concat|map|mapv|filter|filterv|remove|reduce|apply|comp|partial|identity|constantly|juxt|complement|fnil|memoize|sort|sort-by|reverse|flatten|distinct|group-by|partition|partition-by|split-at|split-with|take|drop|take-while|drop-while|interleave|interpose|zipmap|frequencies|count|empty|not-empty|seq|vec|set|hash-map|hash-set|sorted-map|sorted-set|list|vector|str|name|namespace|keyword|symbol|gensym|int|long|float|double|bigint|bigdec|num|inc|dec|pos\\?|neg\\?|zero\\?|even\\?|odd\\?|quot|rem|mod|max|min|abs|rand|rand-int|rand-nth|shuffle|println|print|pr|prn|pr-str|prn-str|format|read|read-string|slurp|spit|atom|deref|reset!|swap!|compare-and-set!|agent|send|send-off|await|ref|dosync|alter|commute|ensure|future|promise|deliver|realized\\?|delay|force|trampoline|type|class|instance\\?|isa\\?|supers|bases|ancestors|descendants|parents|make-hierarchy|derive|underive)\\b" :font-lock-builtin-face]

   ;; Numbers (integers, floats, ratios, hex, octal, binary)
   ["\\b[+-]?[0-9]+\\.?[0-9]*([eE][+-]?[0-9]+)?\\b" :font-lock-number-face]
   ["\\b0[xX][0-9a-fA-F]+\\b" :font-lock-number-face]
   ["\\b[0-9]+/[0-9]+\\b" :font-lock-number-face]

   ;; Boolean literals
   ["\\b(true|false|nil)\\b" :font-lock-constant-face]])

(def javascript-font-lock-keywords
  "Font-lock keywords for JavaScript mode."
  [;; Comments (single-line and multi-line)
   ["//.*$" :font-lock-comment-face]
   ["/\\*[\\s\\S]*?\\*/" :font-lock-comment-face]

   ;; Strings (double and single quoted)
   ["\"(?:[^\"\\\\]|\\\\.)*\"" :font-lock-string-face]
   ["'(?:[^'\\\\]|\\\\.)*'" :font-lock-string-face]

   ;; Template literals
   ["`(?:[^`\\\\]|\\\\.)*`" :font-lock-string-face]

   ;; Keywords
   ["\\b(break|case|catch|const|continue|debugger|default|delete|do|else|export|extends|finally|for|function|if|import|in|instanceof|let|new|return|switch|this|throw|try|typeof|var|void|while|with|yield|async|await|class|static|super|get|set|of|from|as)\\b" :font-lock-keyword-face]

   ;; Function definitions
   ["\\bfunction\\s+([a-zA-Z_$][a-zA-Z0-9_$]*)" 1 :font-lock-function-name-face]

   ;; Built-in objects/functions
   ["\\b(Array|Boolean|Date|Error|Function|JSON|Math|Number|Object|RegExp|String|Symbol|Map|Set|WeakMap|WeakSet|Promise|Proxy|Reflect|console|window|document|process|module|exports|require|global|Buffer|setTimeout|setInterval|clearTimeout|clearInterval|parseInt|parseFloat|isNaN|isFinite|encodeURI|decodeURI|encodeURIComponent|decodeURIComponent|eval)\\b" :font-lock-builtin-face]

   ;; Constants
   ["\\b(true|false|null|undefined|NaN|Infinity)\\b" :font-lock-constant-face]

   ;; Numbers
   ["\\b[0-9]+\\.?[0-9]*([eE][+-]?[0-9]+)?\\b" :font-lock-number-face]
   ["\\b0[xX][0-9a-fA-F]+\\b" :font-lock-number-face]
   ["\\b0[bB][01]+\\b" :font-lock-number-face]
   ["\\b0[oO][0-7]+\\b" :font-lock-number-face]])

(def python-font-lock-keywords
  "Font-lock keywords for Python mode."
  [;; Comments
   ["#.*$" :font-lock-comment-face]

   ;; Triple-quoted strings (must come before regular strings)
   ["\"\"\"[\\s\\S]*?\"\"\"" :font-lock-string-face]
   ["'''[\\s\\S]*?'''" :font-lock-string-face]

   ;; Strings
   ["\"(?:[^\"\\\\]|\\\\.)*\"" :font-lock-string-face]
   ["'(?:[^'\\\\]|\\\\.)*'" :font-lock-string-face]

   ;; Keywords
   ["\\b(and|as|assert|async|await|break|class|continue|def|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|nonlocal|not|or|pass|raise|return|try|while|with|yield)\\b" :font-lock-keyword-face]

   ;; Function definitions
   ["\\bdef\\s+([a-zA-Z_][a-zA-Z0-9_]*)" 1 :font-lock-function-name-face]

   ;; Class definitions
   ["\\bclass\\s+([a-zA-Z_][a-zA-Z0-9_]*)" 1 :font-lock-type-face]

   ;; Built-ins
   ["\\b(abs|all|any|bin|bool|bytes|callable|chr|classmethod|compile|complex|delattr|dict|dir|divmod|enumerate|eval|exec|filter|float|format|frozenset|getattr|globals|hasattr|hash|help|hex|id|input|int|isinstance|issubclass|iter|len|list|locals|map|max|memoryview|min|next|object|oct|open|ord|pow|print|property|range|repr|reversed|round|set|setattr|slice|sorted|staticmethod|str|sum|super|tuple|type|vars|zip|__import__|True|False|None)\\b" :font-lock-builtin-face]

   ;; Decorators
   ["@[a-zA-Z_][a-zA-Z0-9_.]*" :font-lock-preprocessor-face]

   ;; Numbers
   ["\\b[0-9]+\\.?[0-9]*([eE][+-]?[0-9]+)?[jJ]?\\b" :font-lock-number-face]
   ["\\b0[xX][0-9a-fA-F]+\\b" :font-lock-number-face]
   ["\\b0[bB][01]+\\b" :font-lock-number-face]
   ["\\b0[oO][0-7]+\\b" :font-lock-number-face]])

(def rust-font-lock-keywords
  "Font-lock keywords for Rust mode."
  [;; Comments
   ["//.*$" :font-lock-comment-face]
   ["/\\*[\\s\\S]*?\\*/" :font-lock-comment-face]

   ;; Strings (including raw strings)
   ["\"(?:[^\"\\\\]|\\\\.)*\"" :font-lock-string-face]
   ["r#*\"[\\s\\S]*?\"#*" :font-lock-string-face]

   ;; Character literals
   ["'(?:[^'\\\\]|\\\\.)*'" :font-lock-string-face]

   ;; Keywords
   ["\\b(as|async|await|break|const|continue|crate|dyn|else|enum|extern|false|fn|for|if|impl|in|let|loop|match|mod|move|mut|pub|ref|return|self|Self|static|struct|super|trait|true|type|unsafe|use|where|while)\\b" :font-lock-keyword-face]

   ;; Function definitions
   ["\\bfn\\s+([a-zA-Z_][a-zA-Z0-9_]*)" 1 :font-lock-function-name-face]

   ;; Type definitions
   ["\\b(struct|enum|trait|type)\\s+([a-zA-Z_][a-zA-Z0-9_]*)" 2 :font-lock-type-face]

   ;; Built-in types
   ["\\b(bool|char|str|u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|String|Vec|Box|Rc|Arc|Cell|RefCell|Option|Result|Some|None|Ok|Err)\\b" :font-lock-type-face]

   ;; Macros (ending with !)
   ["[a-zA-Z_][a-zA-Z0-9_]*!" :font-lock-preprocessor-face]

   ;; Attributes
   ["#\\[.*?\\]" :font-lock-preprocessor-face]
   ["#!\\[.*?\\]" :font-lock-preprocessor-face]

   ;; Numbers
   ["\\b[0-9][0-9_]*\\.?[0-9_]*([eE][+-]?[0-9_]+)?([fui](8|16|32|64|128|size))?\\b" :font-lock-number-face]
   ["\\b0[xX][0-9a-fA-F_]+([ui](8|16|32|64|128|size))?\\b" :font-lock-number-face]
   ["\\b0[bB][01_]+([ui](8|16|32|64|128|size))?\\b" :font-lock-number-face]
   ["\\b0[oO][0-7_]+([ui](8|16|32|64|128|size))?\\b" :font-lock-number-face]])

(def html-font-lock-keywords
  "Font-lock keywords for HTML mode."
  [;; Comments
   ["<!--[\\s\\S]*?-->" :font-lock-comment-face]

   ;; DOCTYPE
   ["<!DOCTYPE[^>]*>" :font-lock-preprocessor-face]

   ;; Tags
   ["</?[a-zA-Z][a-zA-Z0-9-]*" :font-lock-keyword-face]
   ["/?>" :font-lock-keyword-face]

   ;; Attributes
   ["\\s([a-zA-Z][a-zA-Z0-9-]*)=" 1 :font-lock-variable-name-face]

   ;; Attribute values (quoted)
   ["=\"[^\"]*\"" :font-lock-string-face]
   ["='[^']*'" :font-lock-string-face]])

(def css-font-lock-keywords
  "Font-lock keywords for CSS mode."
  [;; Comments
   ["/\\*[\\s\\S]*?\\*/" :font-lock-comment-face]

   ;; Selectors (before {)
   ["[a-zA-Z][a-zA-Z0-9_-]*(?=\\s*\\{)" :font-lock-function-name-face]
   ["\\.[a-zA-Z][a-zA-Z0-9_-]*" :font-lock-type-face]
   ["#[a-zA-Z][a-zA-Z0-9_-]*" :font-lock-constant-face]

   ;; Properties
   ["[a-zA-Z-]+(?=\\s*:)" :font-lock-variable-name-face]

   ;; Values (colors, units)
   ["#[0-9a-fA-F]{3,8}\\b" :font-lock-constant-face]
   ["\\b[0-9]+\\.?[0-9]*(px|em|rem|%|vh|vw|pt|cm|mm|in|deg|rad|s|ms)?\\b" :font-lock-number-face]

   ;; Strings
   ["\"[^\"]*\"" :font-lock-string-face]
   ["'[^']*'" :font-lock-string-face]

   ;; Keywords
   ["\\b(important|inherit|initial|unset|auto|none)\\b" :font-lock-keyword-face]

   ;; At-rules
   ["@[a-zA-Z][a-zA-Z0-9-]*" :font-lock-preprocessor-face]])

(def markdown-font-lock-keywords
  "Font-lock keywords for Markdown mode."
  [;; Headers
   ["^#{1,6}\\s.*$" :font-lock-function-name-face]

   ;; Code blocks (fenced)
   ["```[\\s\\S]*?```" :font-lock-string-face]

   ;; Inline code
   ["`[^`]+`" :font-lock-string-face]

   ;; Bold
   ["\\*\\*[^*]+\\*\\*" :font-lock-keyword-face]
   ["__[^_]+__" :font-lock-keyword-face]

   ;; Italic
   ["\\*[^*]+\\*" :font-lock-variable-name-face]
   ["_[^_]+_" :font-lock-variable-name-face]

   ;; Links
   ["\\[[^\\]]+\\]\\([^)]+\\)" :font-lock-constant-face]

   ;; Lists
   ["^\\s*[-*+]\\s" :font-lock-builtin-face]
   ["^\\s*[0-9]+\\.\\s" :font-lock-builtin-face]

   ;; Blockquotes
   ["^>.*$" :font-lock-comment-face]])

;; =============================================================================
;; Mode to Keywords Mapping
;; =============================================================================

(def mode-keywords
  "Map of major modes to their font-lock keywords."
  {:clojure-mode clojure-font-lock-keywords
   :clojurescript-mode clojure-font-lock-keywords
   :javascript-mode javascript-font-lock-keywords
   :js-mode javascript-font-lock-keywords
   :python-mode python-font-lock-keywords
   :rust-mode rust-font-lock-keywords
   :html-mode html-font-lock-keywords
   :css-mode css-font-lock-keywords
   :markdown-mode markdown-font-lock-keywords
   :fundamental-mode []})

;; =============================================================================
;; Fontification Engine
;; =============================================================================

(defn- multiline-pattern?
  "Check if a regex pattern contains multiline constructs."
  [pattern-str]
  (or (str/includes? pattern-str "[\\s\\S]")
      (str/includes? pattern-str "\\n")
      (str/includes? pattern-str "[^]*")))

(defn- compile-keyword
  "Compile a font-lock keyword spec to an efficient form.
   Returns {:regex Pattern :face keyword :group int :multiline? bool}.

   Issue #144: Patterns containing multiline constructs (e.g. [\\s\\S]*?)
   are compiled with 'gs' flags to enable dotAll matching."
  [[pattern & rest]]
  (let [pattern-str (if (string? pattern) pattern (str pattern))
        ;; Issue #144: Detect multiline patterns and use appropriate flags
        is-multiline (multiline-pattern? pattern-str)
        flags (if is-multiline "gms" "gm")]
    (cond
      ;; [regex face] - simple form
      (keyword? (first rest))
      {:regex (js/RegExp. pattern-str flags)
       :face (first rest)
       :group 0
       :multiline? is-multiline}

      ;; [regex group face] - group form
      (and (number? (first rest)) (keyword? (second rest)))
      {:regex (js/RegExp. pattern-str flags)
       :face (second rest)
       :group (first rest)
       :multiline? is-multiline}

      ;; Default: treat as simple
      :else
      {:regex (js/RegExp. pattern-str flags)
       :face (first rest)
       :group 0
       :multiline? is-multiline})))

(defn- find-matches
  "Find all matches for a compiled keyword in text.
   Returns seq of {:start :end :face}."
  [text {:keys [regex face group]}]
  (let [results (atom [])]
    ;; Reset regex state
    (set! (.-lastIndex regex) 0)
    (loop []
      (when-let [match (.exec regex text)]
        (let [full-match (aget match 0)
              group-match (if (zero? group)
                            full-match
                            (aget match group))
              start (if (zero? group)
                      (.-index match)
                      ;; Calculate group start position
                      (let [before-group (subs full-match 0
                                               (.indexOf full-match group-match))]
                        (+ (.-index match) (count before-group))))
              end (+ start (count (or group-match "")))]
          (when (and group-match (pos? (count group-match)))
            (swap! results conj {:start start :end end :face face}))
          (recur))))
    @results))

(defn- extend-region-for-multiline
  "Extend region boundaries to safe positions for multiline constructs.
   Moves start backward and end forward to avoid splitting multiline patterns.

   Issue #144: Prevents incorrect highlighting when multiline constructs
   (strings, comments, etc.) span across region boundaries."
  [text region-start region-end]
  (let [text-len (count text)
        ;; Extend start backward: find beginning of any multiline construct
        ;; Look for unmatched opening delimiters (triple-quotes, /*, `)
        extended-start (loop [pos region-start]
                         (if (<= pos 0)
                           0
                           ;; Check if we're inside a multiline string/comment
                           (let [ch (nth text (dec pos) nil)]
                             (if (and ch (not (#{\newline} ch)))
                               (recur (dec pos))
                               pos))))
        ;; Extend end forward: find end of any multiline construct
        extended-end (loop [pos region-end]
                       (if (>= pos text-len)
                         text-len
                         (let [ch (nth text pos nil)]
                           (if (and ch (not (#{\newline} ch)))
                             (recur (inc pos))
                             pos))))]
    [extended-start extended-end]))

(defn fontify-region
  "Apply font-lock keywords to a region of text.
   Returns updated text-properties map with :face properties set.

   Issue #144: Supports multiline patterns by extending region boundaries
   to avoid splitting multiline constructs (strings, comments, etc.).

   Args:
     text - The buffer text string
     text-props - Current text properties map
     region-start - Start position (inclusive)
     region-end - End position (exclusive)
     keywords - Vector of font-lock keyword specs

   Returns:
     Updated text-properties map."
  [text text-props region-start region-end keywords]
  (if (empty? keywords)
    text-props
    (let [;; Issue #144: Check if any keywords are multiline
          compiled (map compile-keyword keywords)
          has-multiline? (some :multiline? compiled)
          ;; Extend region if multiline patterns exist
          [effective-start effective-end]
          (if has-multiline?
            (extend-region-for-multiline text region-start region-end)
            [region-start region-end])
          ;; Clamp to text bounds
          effective-start (max 0 effective-start)
          effective-end (min (count text) effective-end)
          ;; Extract the region text
          region-text (subs text effective-start effective-end)
          ;; Find all matches
          all-matches (mapcat (partial find-matches region-text) compiled)
          ;; Sort by start position, then by specificity (longer match wins)
          sorted-matches (sort-by (juxt :start #(- (:end %) (:start %))) all-matches)]
      ;; Apply matches, later matches don't override earlier ones
      (reduce
       (fn [props {:keys [start end face]}]
         (let [abs-start (+ effective-start start)
               abs-end (+ effective-start end)
               existing (text-props/get-text-property props abs-start :face)]
           ;; Only apply if no face already set (first match wins)
           (if existing
             props
             (text-props/put-text-property props abs-start abs-end :face face))))
       text-props
       sorted-matches))))

(defn fontify-buffer
  "Apply font-lock keywords to an entire buffer.

   Args:
     text - The buffer text string
     keywords - Vector of font-lock keyword specs

   Returns:
     Text-properties map with :face properties."
  [text keywords]
  (fontify-region text {} 0 (count text) keywords))

(defn unfontify-region
  "Remove font-lock face properties from a region.

   Args:
     text-props - Current text properties map
     start - Start position
     end - End position

   Returns:
     Updated text-properties map."
  [text-props start end]
  (text-props/remove-text-properties text-props start end :face))

;; =============================================================================
;; Mode Detection and Keywords
;; =============================================================================

(defn get-keywords-for-mode
  "Get font-lock keywords for a major mode.
   Falls back to empty vector for unknown modes."
  [mode]
  (get mode-keywords mode []))

(defn detect-mode-from-filename
  "Detect major mode from filename extension.
   Returns mode keyword or :fundamental-mode."
  [filename]
  (when filename
    (let [ext (-> filename
                  (str/lower-case)
                  (str/split #"\.")
                  last)]
      (case ext
        ("clj" "cljs" "cljc" "edn") :clojure-mode
        ("js" "mjs" "cjs") :javascript-mode
        ("ts" "tsx") :javascript-mode  ; TypeScript uses similar keywords
        ("jsx") :javascript-mode
        ("py" "pyw") :python-mode
        ("rs") :rust-mode
        ("html" "htm") :html-mode
        ("css" "scss" "less") :css-mode
        ("md" "markdown") :markdown-mode
        :fundamental-mode))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :font-lock/fontify-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Fontify an entire buffer based on its major mode."
   (let [buffer (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         mode (or (:major-mode buffer) :fundamental-mode)
         keywords (get-keywords-for-mode mode)]
     (if (and wasm-instance (seq keywords))
       (let [text (.getText wasm-instance)
             new-props (fontify-buffer text keywords)]
         {:db (assoc-in db [:buffers buffer-id :text-properties]
                        (merge (get-in db [:buffers buffer-id :text-properties] {})
                               new-props))})
       {:db db}))))

(rf/reg-event-fx
 :font-lock/fontify-region
 (fn [{:keys [db]} [_ buffer-id region-start region-end]]
   "Fontify a region of a buffer. Used after edits for incremental updates."
   (let [buffer (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         mode (or (:major-mode buffer) :fundamental-mode)
         keywords (get-keywords-for-mode mode)]
     (if (and wasm-instance (seq keywords))
       (let [text (.getText wasm-instance)
             ;; Extend region to line boundaries for proper highlighting
             lines (str/split text #"\n" -1)
             {:keys [line]} (loop [pos 0 ln 0]
                              (if (>= pos region-start)
                                {:line ln}
                                (let [line-len (inc (count (nth lines ln "")))]
                                  (recur (+ pos line-len) (inc ln)))))
             line-start (reduce + (map #(inc (count %)) (take line lines)))
             ;; Find end of line containing 'region-end'
             {:keys [end-line]} (loop [pos 0 l 0]
                                  (if (>= pos region-end)
                                    {:end-line l}
                                    (let [line-len (inc (count (nth lines l "")))]
                                      (recur (+ pos line-len) (inc l)))))
             line-end (reduce + (map #(inc (count %)) (take (inc end-line) lines)))
             ;; Clear face properties in the extended region
             cleared-props (unfontify-region
                            (get-in db [:buffers buffer-id :text-properties] {})
                            line-start line-end)
             ;; Re-fontify the region
             new-props (fontify-region text cleared-props line-start line-end keywords)]
         {:db (assoc-in db [:buffers buffer-id :text-properties] new-props)})
       {:db db}))))

(rf/reg-event-fx
 :font-lock/enable
 (fn [{:keys [db]} [_ buffer-id]]
   "Enable font-lock mode for a buffer."
   {:db (assoc-in db [:buffers buffer-id :font-lock-mode] true)
    :fx [[:dispatch [:font-lock/fontify-buffer buffer-id]]]}))

(rf/reg-event-fx
 :font-lock/disable
 (fn [{:keys [db]} [_ buffer-id]]
   "Disable font-lock mode for a buffer."
   {:db (-> db
            (assoc-in [:buffers buffer-id :font-lock-mode] false)
            (update-in [:buffers buffer-id :text-properties] dissoc :face))}))

(rf/reg-event-fx
 :font-lock/toggle
 (fn [{:keys [db]} [_ buffer-id]]
   "Toggle font-lock mode for a buffer."
   (let [enabled? (get-in db [:buffers buffer-id :font-lock-mode] false)]
     {:fx [[:dispatch [(if enabled?
                         :font-lock/disable
                         :font-lock/enable) buffer-id]]]})))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :font-lock/enabled?
 (fn [db [_ buffer-id]]
   (get-in db [:buffers buffer-id :font-lock-mode] false)))

(rf/reg-sub
 :font-lock/keywords
 (fn [db [_ mode]]
   (get-keywords-for-mode mode)))

;; =============================================================================
;; Global Font-Lock Mode
;; =============================================================================

(rf/reg-event-db
 :font-lock/set-global
 (fn [db [_ enabled?]]
   "Enable/disable global font-lock mode.
    When enabled, font-lock is automatically enabled for new buffers."
   (assoc-in db [:settings :global-font-lock-mode] enabled?)))

(rf/reg-sub
 :font-lock/global-enabled?
 (fn [db _]
   (get-in db [:settings :global-font-lock-mode] true)))  ; Default to enabled

;; =============================================================================
;; Interactive Commands
;; =============================================================================

(defn font-lock-mode-toggle!
  "Toggle syntax highlighting for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)]
    (rf/dispatch [:font-lock/toggle buffer-id])))

(defn global-font-lock-mode-toggle!
  "Toggle global syntax highlighting."
  []
  (let [enabled? @(rf/subscribe [:font-lock/global-enabled?])]
    (rf/dispatch [:font-lock/set-global (not enabled?)])))

;; =============================================================================
;; JIT Fontification (Issue #143)
;; =============================================================================

;; JIT (just-in-time) fontification defers highlighting to when regions
;; become visible, rather than fontifying the entire buffer upfront.
;; This is critical for large files (>10K lines).

(def jit-lock-chunk-size
  "Number of characters to fontify per chunk."
  3000)

(def jit-lock-context-lines
  "Extra lines above/below viewport to fontify for scroll context."
  50)

(defonce jit-idle-timer (atom nil))

(defn- cancel-jit-timer!
  "Cancel any pending JIT fontification timer."
  []
  (when-let [t @jit-idle-timer]
    (js/clearTimeout t)
    (reset! jit-idle-timer nil)))

(defn- schedule-jit-fontification!
  "Schedule JIT fontification after a brief idle delay."
  [buffer-id]
  (cancel-jit-timer!)
  (reset! jit-idle-timer
          (js/setTimeout
           (fn [] (rf/dispatch [:font-lock/jit-fontify buffer-id]))
           100)))

(rf/reg-event-fx
 :font-lock/jit-fontify
 (fn [{:keys [db]} [_ buffer-id]]
   "JIT-fontify the visible region of a buffer plus context.
    Issue #143: Only fontifies what's needed, not the entire buffer."
   (let [buffer (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         mode (or (:major-mode buffer) :fundamental-mode)
         keywords (get-keywords-for-mode mode)
         cursor-pos (get-in db [:ui :cursor-position] 0)]
     (if (and wasm-instance (seq keywords))
       (let [text (.getText wasm-instance)
             text-len (count text)
             ;; Estimate visible region around cursor
             ;; Use lines to determine range
             lines (str/split text #"\n" -1)
             ;; Find which line cursor is on
             cursor-line (loop [pos 0 ln 0]
                           (if (or (>= pos cursor-pos) (>= ln (count lines)))
                             ln
                             (recur (+ pos 1 (count (nth lines ln ""))) (inc ln))))
             ;; Visible range with context
             start-line (max 0 (- cursor-line jit-lock-context-lines))
             end-line (min (count lines) (+ cursor-line jit-lock-context-lines))
             ;; Convert line range to character positions
             region-start (reduce + 0 (map #(inc (count %)) (take start-line lines)))
             region-end (min text-len
                             (reduce + 0 (map #(inc (count %)) (take end-line lines))))
             ;; Check if this region is already fontified
             fontified-ranges (get-in db [:buffers buffer-id :fontified-ranges] #{})
             range-key [region-start region-end]
             already-fontified? (contains? fontified-ranges range-key)]
         (if already-fontified?
           {:db db}
           (let [;; Clear existing props in range first
                 cleared-props (unfontify-region
                                (get-in db [:buffers buffer-id :text-properties] {})
                                region-start region-end)
                 ;; Fontify the visible region
                 new-props (fontify-region text cleared-props region-start region-end keywords)]
             {:db (-> db
                      (assoc-in [:buffers buffer-id :text-properties] new-props)
                      (update-in [:buffers buffer-id :fontified-ranges]
                                 (fnil conj #{}) range-key))})))
       {:db db}))))

(rf/reg-event-fx
 :font-lock/on-scroll
 (fn [{:keys [db]} [_ buffer-id]]
   "Trigger JIT fontification on scroll.
    Issue #143: Schedules deferred fontification for visible region."
   (let [font-lock-on? (get-in db [:buffers buffer-id :font-lock-mode] false)
         jit-enabled? (get-in db [:settings :jit-lock-mode] true)]
     (when (and font-lock-on? jit-enabled?)
       (schedule-jit-fontification! buffer-id))
     {:db db})))

(rf/reg-event-fx
 :font-lock/invalidate-region
 (fn [{:keys [db]} [_ buffer-id region-start region-end]]
   "Invalidate fontification cache for a region after text changes.
    Issue #143: Marks region as needing re-fontification."
   {:db (-> db
            ;; Clear fontified ranges that overlap with changed region
            (update-in [:buffers buffer-id :fontified-ranges]
                       (fn [ranges]
                         (set (remove (fn [[rs re]]
                                        (and (< rs region-end) (> re region-start)))
                                      (or ranges #{})))))
            ;; Clear face properties in the changed region
            (update-in [:buffers buffer-id :text-properties]
                       (fn [props]
                         (if props
                           (unfontify-region props region-start region-end)
                           {}))))}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-font-lock!
  "Initialize font-lock module and register commands."
  []
  (lisp/define-command 'font-lock-mode
    font-lock-mode-toggle!
    "Toggle syntax highlighting for current buffer")

  (lisp/define-command 'global-font-lock-mode
    global-font-lock-mode-toggle!
    "Toggle global syntax highlighting"))

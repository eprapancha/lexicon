(ns lexicon.core.api.interactive
  "Interactive specification parser for Emacs-compatible command definitions.

   Parses interactive spec strings like \"p\", \"r\", \"sPrompt: \" and returns
   a plan for collecting arguments before executing the command.

   Spec format: String of code letters separated by \\n
   Example: \"r\\nsPattern: \" => region + string prompt

   Based on Emacs src/callint.c implementation."
  (:require [clojure.string :as str]
            [lexicon.core.api.buffer :as buf]
            [lexicon.core.commands.universal-argument :as universal-arg]))

;; -- Interactive Spec Parser --

(defn parse-interactive-spec
  "Parse an interactive spec string into argument collection plan.

   Input: Spec string (e.g., \"p\", \"r\\nsSearch: \", \"P\\nr\\nsPrompt: \")
   Output: Vector of argument descriptors

   Argument descriptor:
   {:code \"p\"                   ; Code letter
    :type :prefix-arg-number    ; Argument type
    :prompt nil                 ; Prompt string (if any)
    :io? false}                 ; Whether this requires I/O

   Supported codes (Tier 1):
   - p: Prefix arg converted to number (no I/O)
   - P: Prefix arg in raw form (no I/O)
   - r: Region start and end as two numbers (no I/O) - produces 2 args!
   - R: Active region (like r but nil unless region active) (no I/O)
   - s: String from minibuffer
   - b: Buffer name (existing)
   - B: Buffer name (possibly nonexistent)
   - f: File name (existing)
   - F: File name (possibly nonexistent)
   - d: Value of point (no I/O)
   - m: Value of mark (no I/O)
   - n: Number from minibuffer
   - i: Ignored (always nil, no I/O)"
  [spec-string]
  (when (and spec-string (not= spec-string ""))
    ;; Split by newline - each segment is one arg spec
    (let [segments (str/split spec-string #"\n")]
      (vec
       (mapcat
        (fn [segment]
          (when-not (empty? segment)
            (let [code (first segment)
                  prompt (when (> (count segment) 1)
                          (subs segment 1))]  ; Rest after code letter
              (case code
                ;; Prefix argument codes (no I/O)
                \p [{:code "p"
                     :type :prefix-arg-number
                     :prompt nil
                     :io? false}]

                \P [{:code "P"
                     :type :prefix-arg-raw
                     :prompt nil
                     :io? false}]

                ;; Region codes (no I/O) - produce TWO arguments!
                \r [{:code "r"
                     :type :region-start
                     :prompt nil
                     :io? false}
                    {:code "r"
                     :type :region-end
                     :prompt nil
                     :io? false}]

                \R [{:code "R"
                     :type :active-region-start
                     :prompt nil
                     :io? false}
                    {:code "R"
                     :type :active-region-end
                     :prompt nil
                     :io? false}]

                ;; String from minibuffer
                \s [{:code "s"
                     :type :string
                     :prompt (or prompt "String: ")
                     :io? true}]

                ;; Buffer name
                \b [{:code "b"
                     :type :buffer-name
                     :prompt (or prompt "Buffer: ")
                     :io? true}]

                \B [{:code "B"
                     :type :buffer-name-or-new
                     :prompt (or prompt "Buffer: ")
                     :io? true}]

                ;; File name
                \f [{:code "f"
                     :type :file-name
                     :prompt (or prompt "File: ")
                     :io? true}]

                \F [{:code "F"
                     :type :file-name-or-new
                     :prompt (or prompt "File: ")
                     :io? true}]

                ;; Point/mark (no I/O)
                \d [{:code "d"
                     :type :point
                     :prompt nil
                     :io? false}]

                \m [{:code "m"
                     :type :mark
                     :prompt nil
                     :io? false}]

                ;; Number from minibuffer
                \n [{:code "n"
                     :type :number
                     :prompt (or prompt "Number: ")
                     :io? true}]

                ;; Ignored (always nil)
                \i [{:code "i"
                     :type :ignore
                     :prompt nil
                     :io? false}]

                ;; Unknown code - ignore for now
                (do
                  (js/console.warn "Unknown interactive code:" code)
                  [])))))
        segments)))))

;; -- Argument Collection --

(defn collect-arg
  "Collect a single argument according to its descriptor.

   Returns:
   - For non-I/O args: {:type :immediate :value <val>}
   - For I/O args: {:type :async :prompt \"...\" :read-fn <fn>}

   The :async type requires minibuffer interaction."
  [db arg-descriptor]
  (case (:type arg-descriptor)
    ;; Prefix argument (converted to number) - Phase 6.5
    ;; Use prefix-numeric-value from universal-argument namespace
    :prefix-arg-number
    (let [current-prefix-arg (:current-prefix-arg db)]
      {:type :immediate
       :value (universal-arg/prefix-numeric-value current-prefix-arg)})

    ;; Prefix argument (raw form) - Phase 6.5
    :prefix-arg-raw
    (let [current-prefix-arg (:current-prefix-arg db)]
      {:type :immediate
       :value current-prefix-arg})

    ;; Region start
    :region-start
    (let [active-window (lexicon.core.db/find-window-in-tree
                         (:window-tree db) (:active-window-id db))
          cursor-pos (:cursor-position active-window)
          mark-pos (:mark-position active-window)]
      (if (and cursor-pos mark-pos)
        (let [cursor-linear (buf/line-col-to-point
                            (get (:buffers db) (:buffer-id active-window))
                            (:line cursor-pos) (:column cursor-pos))
              mark-linear (buf/line-col-to-point
                          (get (:buffers db) (:buffer-id active-window))
                          (:line mark-pos) (:column mark-pos))]
          {:type :immediate
           :value (min cursor-linear mark-linear)})
        {:type :immediate :value nil}))

    ;; Region end
    :region-end
    (let [active-window (lexicon.core.db/find-window-in-tree
                         (:window-tree db) (:active-window-id db))
          cursor-pos (:cursor-position active-window)
          mark-pos (:mark-position active-window)]
      (if (and cursor-pos mark-pos)
        (let [cursor-linear (buf/line-col-to-point
                            (get (:buffers db) (:buffer-id active-window))
                            (:line cursor-pos) (:column cursor-pos))
              mark-linear (buf/line-col-to-point
                          (get (:buffers db) (:buffer-id active-window))
                          (:line mark-pos) (:column mark-pos))]
          {:type :immediate
           :value (max cursor-linear mark-linear)})
        {:type :immediate :value nil}))

    ;; Active region (nil unless region active)
    :active-region-start
    (let [active-window (lexicon.core.db/find-window-in-tree
                         (:window-tree db) (:active-window-id db))
          mark-pos (:mark-position active-window)]
      (if mark-pos
        (collect-arg db (assoc arg-descriptor :type :region-start))
        {:type :immediate :value nil}))

    :active-region-end
    (let [active-window (lexicon.core.db/find-window-in-tree
                         (:window-tree db) (:active-window-id db))
          mark-pos (:mark-position active-window)]
      (if mark-pos
        (collect-arg db (assoc arg-descriptor :type :region-end))
        {:type :immediate :value nil}))

    ;; Point (current cursor position as number)
    :point
    {:type :immediate
     :value (buf/point db)}

    ;; Mark (mark position as number)
    :mark
    (let [active-window (lexicon.core.db/find-window-in-tree
                         (:window-tree db) (:active-window-id db))
          mark-pos (:mark-position active-window)]
      {:type :immediate
       :value (when mark-pos
                (buf/line-col-to-point
                 (get (:buffers db) (:buffer-id active-window))
                 (:line mark-pos) (:column mark-pos)))})

    ;; Ignored (always nil)
    :ignore
    {:type :immediate :value nil}

    ;; String from minibuffer
    :string
    {:type :async
     :prompt (:prompt arg-descriptor)
     :read-fn :read-string
     :completion nil}

    ;; Buffer name (existing)
    :buffer-name
    {:type :async
     :prompt (:prompt arg-descriptor)
     :read-fn :read-buffer
     :completion (vec (map :name (vals (:buffers db))))}

    ;; Buffer name (possibly nonexistent)
    :buffer-name-or-new
    {:type :async
     :prompt (:prompt arg-descriptor)
     :read-fn :read-buffer-allow-new
     :completion (vec (map :name (vals (:buffers db))))}

    ;; File name (existing)
    :file-name
    {:type :async
     :prompt (:prompt arg-descriptor)
     :read-fn :read-file-name
     :completion nil}  ; File completion TBD

    ;; File name (possibly nonexistent)
    :file-name-or-new
    {:type :async
     :prompt (:prompt arg-descriptor)
     :read-fn :read-file-name-allow-new
     :completion nil}

    ;; Number from minibuffer
    :number
    {:type :async
     :prompt (:prompt arg-descriptor)
     :read-fn :read-number
     :completion nil}

    ;; Default: nil
    {:type :immediate :value nil}))

(defn collect-all-args
  "Collect all arguments according to the interactive spec.

   Accepts two formats:
   1. Emacs string format: \"p\", \"r\\nsPattern: \", etc.
   2. Lexicon vector format: [{:type :async :prompt \"...\"}]

   Returns:
   {:immediate-args [val1 val2 ...]   ; Arguments available immediately
    :async-prompts [{:prompt ... :read-fn ... :index N} ...]  ; Args needing I/O
    :total-args N}                     ; Total number of args expected

   If :async-prompts is empty, all args are ready.
   Otherwise, need to prompt user for remaining args."
  [db spec]
  (cond
    ;; Vector format (Lexicon custom format) - use directly as async prompts
    (vector? spec)
    {:immediate-args []
     :async-prompts (vec (keep-indexed
                          (fn [idx prompt-desc]
                            (when (= (:type prompt-desc) :async)
                              (assoc prompt-desc :index idx)))
                          spec))
     :total-args (count spec)}

    ;; String format (Emacs format) - parse and collect
    (string? spec)
    (let [arg-descriptors (parse-interactive-spec spec)
          collected (mapv #(collect-arg db %) arg-descriptors)
          immediate-args (vec (map :value (filter #(= (:type %) :immediate) collected)))
          async-args (vec (keep-indexed
                           (fn [idx coll]
                             (when (= (:type coll) :async)
                               (assoc coll :index idx)))
                           collected))]
      {:immediate-args immediate-args
       :async-prompts async-args
       :total-args (count arg-descriptors)})

    ;; Nil or unknown format
    :else
    {:immediate-args []
     :async-prompts []
     :total-args 0}))

;; -- Helper Functions --

(defn needs-minibuffer?
  "Check if an interactive spec requires minibuffer interaction."
  [spec-string]
  (let [descriptors (parse-interactive-spec spec-string)]
    (boolean (some :io? descriptors))))

(defn validate-interactive-spec
  "Validate an interactive spec string.
   Returns nil if valid, error message if invalid."
  [spec-string]
  (try
    (parse-interactive-spec spec-string)
    nil
    (catch js/Error e
      (str "Invalid interactive spec: " (.-message e)))))

;; -- Examples for Documentation --

(comment
  ;; Example 1: Simple prefix argument
  (parse-interactive-spec "p")
  ;; => [{:code "p" :type :prefix-arg-number :prompt nil :io? false}]

  ;; Example 2: Region + string prompt
  (parse-interactive-spec "r\nsSearch: ")
  ;; => [{:code "r" :type :region-start :prompt nil :io? false}
  ;;     {:code "r" :type :region-end :prompt nil :io? false}
  ;;     {:code "s" :type :string :prompt "Search: " :io? true}]

  ;; Example 3: Prefix arg + region + pattern
  (parse-interactive-spec "P\nr\nsPattern: ")
  ;; => [{:code "P" :type :prefix-arg-raw :prompt nil :io? false}
  ;;     {:code "r" :type :region-start :prompt nil :io? false}
  ;;     {:code "r" :type :region-end :prompt nil :io? false}
  ;;     {:code "s" :type :string :prompt "Pattern: " :io? true}]

  ;; Collecting args
  (collect-all-args @re-frame.db/app-db "p")
  ;; => {:immediate-args [1] :async-prompts [] :total-args 1}

  (collect-all-args @re-frame.db/app-db "r\nsPattern: ")
  ;; => {:immediate-args [0 100] ; region start/end
  ;;     :async-prompts [{:prompt "Pattern: " :read-fn :read-string :index 2}]
  ;;     :total-args 3}
  )

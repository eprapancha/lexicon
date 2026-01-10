(ns lexicon.events.command
  "Command system and help system event handlers"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.completion.metadata :as completion-metadata]
            [lexicon.advanced-undo :as undo]
            [lexicon.context :as ctx]
            [lexicon.api.interactive :as interactive]))

;; =============================================================================
;; Command Registry and Execution
;; =============================================================================

(rf/reg-event-db
 :register-command
 (fn [db [_ command-name command-definition]]
   "Register a new command in the central command registry.
    Command definition can include:
    {:handler [:event-vector ...]
     :interactive \"p\"           ; Interactive spec string (optional)
     :doc \"Documentation\"}"
   (assoc-in db [:commands command-name] command-definition)))

(rf/reg-event-fx
 :execute-command-interactive
 (fn [{:keys [db]} [_ command-name]]
   "Execute a command with interactive argument collection.
    If the command has an :interactive spec, collect args first.
    If all args immediate (no I/O), dispatch directly.
    If args need I/O, prompt user via minibuffer."
   (let [command-def (get-in db [:commands command-name])
         interactive-spec (:interactive command-def)]
     (if interactive-spec
       ;; Has interactive spec - collect args
       (let [collected (interactive/collect-all-args db interactive-spec)
             immediate-args (:immediate-args collected)
             async-prompts (:async-prompts collected)]
         (if (empty? async-prompts)
           ;; All args ready - execute immediately
           {:fx [[:dispatch (into [:execute-command command-name] immediate-args)]]}
           ;; Need to prompt user - activate minibuffer for first async arg
           (let [first-prompt (first async-prompts)]
             {:db (assoc-in db [:command-execution :pending]
                           {:command command-name
                            :immediate-args immediate-args
                            :remaining-prompts (vec (rest async-prompts))
                            :collected-async-args []})
              :fx [[:dispatch [:minibuffer/activate
                              {:prompt (:prompt first-prompt)
                               :on-confirm [:command/collect-next-arg]}]]]})))
       ;; No interactive spec - execute with no args
       {:fx [[:dispatch [:execute-command command-name]]]}))))

(rf/reg-event-fx
 :command/collect-next-arg
 (fn [{:keys [db]} [_ user-input]]
   "Collect next argument from user input and continue execution."
   (let [pending (get-in db [:command-execution :pending])
         {:keys [command immediate-args remaining-prompts collected-async-args]} pending
         ;; Add user input to collected args
         new-collected (conj collected-async-args user-input)]
     (if (empty? remaining-prompts)
       ;; All args collected - execute command
       (let [all-args (concat immediate-args new-collected)]
         {:db (update db :command-execution dissoc :pending)
          :fx [[:dispatch (into [:execute-command command] all-args)]]})
       ;; More prompts needed
       (let [next-prompt (first remaining-prompts)]
         {:db (assoc-in db [:command-execution :pending]
                       {:command command
                        :immediate-args immediate-args
                        :remaining-prompts (vec (rest remaining-prompts))
                        :collected-async-args new-collected})
          :fx [[:dispatch [:minibuffer/activate
                          {:prompt (:prompt next-prompt)
                           :on-confirm [:command/collect-next-arg]}]]]})))))

(rf/reg-event-fx
 :execute-command
 (fn [{:keys [db]} [_ command-name & args]]
   "Execute a command by name from the central registry with before/after hooks and undo boundaries"
   (let [command-def (get-in db [:commands command-name])
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         prefix-arg (get-in db [:ui :prefix-argument])

         ;; Context for hooks
         context {:command-id command-name
                  :args args
                  :buffer-id active-buffer-id
                  :window-id (:active-window-id db)
                  :prefix-arg prefix-arg
                  :timestamp (js/Date.now)}

         ;; Dynamic execution context
         exec-context {:command command-name
                       :buffer active-buffer-id
                       :prefix-arg prefix-arg}

         ;; Determine if this is an editing command (should create undo boundary)
         editing-command? (not (#{:undo :redo :universal-argument :execute-extended-command
                                  :describe-bindings :forward-char :backward-char :next-line :previous-line
                                  :beginning-of-line :end-of-line :beginning-of-buffer :end-of-buffer} command-name))]

     (if command-def
       (let [handler (:handler command-def)
             should-clear-prefix? (not= command-name :universal-argument)]
         ;; Execute command with undo boundaries for editing commands
         {:fx (cond-> [[:dispatch-with-context {:event [:hook/run :before-command-hook context]
                                                 :context exec-context}]]
                ;; Insert undo boundary before editing commands
                editing-command? (conj [:dispatch [:command/begin-undo-boundary active-buffer-id]])
                ;; Execute the command with dynamic context binding
                true (conj [:dispatch-with-context {:event (into handler args)
                                                    :context exec-context}])
                ;; Insert undo boundary after editing commands
                editing-command? (conj [:dispatch [:command/end-undo-boundary active-buffer-id]])
                ;; Run after-command hook within dynamic context
                true (conj [:dispatch-with-context {:event [:hook/run :after-command-hook (assoc context :result nil)]
                                                    :context exec-context}])
                ;; Clear prefix argument if needed
                should-clear-prefix? (conj [:dispatch [:clear-prefix-argument]]))})
       {:fx [[:dispatch [:show-error (str "Command not found: " command-name)]]]}))))

;; -- Command Lifecycle Undo Integration --

(rf/reg-event-db
  :command/begin-undo-boundary
  (fn [db [_ buffer-id]]
    "Insert undo boundary at beginning of command"
    (when buffer-id
      (undo/undo-boundary! buffer-id))
    db))

(rf/reg-event-db
  :command/end-undo-boundary
  (fn [db [_ buffer-id]]
    "Insert undo boundary at end of command"
    (when buffer-id
      (undo/undo-boundary! buffer-id))
    db))

(rf/reg-event-fx
 :execute-extended-command
 (fn [{:keys [db]} [_]]
   "Open minibuffer for M-x command execution with completion metadata"
   (let [;; Get all registered command names
         command-names (map name (keys (:commands db)))
         ;; Create metadata for command completion
         metadata (completion-metadata/make-metadata
                   :category :command
                   :annotation-function :command
                   :display-sort-function :alphabetical)]
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt "M-x "
                        :completions command-names
                        :metadata metadata
                        :on-confirm [:execute-command-by-name]}]]]})))

(rf/reg-event-fx
 :execute-command-by-name
 (fn [{:keys [db]} [_ command-name-str]]
   "Execute a command by its string name"
   (let [command-keyword (keyword command-name-str)]
     {:fx [[:dispatch [:execute-command command-keyword]]]})))

;; =============================================================================
;; Prefix Argument (Universal Argument)
;; =============================================================================

(rf/reg-event-db
 :universal-argument
 (fn [db [_]]
   "Set or multiply the universal argument (C-u)"
   (let [current-prefix (get-in db [:ui :prefix-argument])
         prefix-active? (get-in db [:ui :prefix-argument-active?])]
     (if prefix-active?
       ;; Already active, multiply by 4
       (-> db
           (assoc-in [:ui :prefix-argument] (* (or current-prefix 4) 4)))
       ;; Not active, set to 4
       (-> db
           (assoc-in [:ui :prefix-argument] 4)
           (assoc-in [:ui :prefix-argument-active?] true)
           (assoc-in [:echo-area :message] "C-u "))))))

(rf/reg-event-db
 :clear-prefix-argument
 (fn [db [_]]
   "Clear the prefix argument after command execution"
   (-> db
       (assoc-in [:ui :prefix-argument] nil)
       (assoc-in [:ui :prefix-argument-active?] false))))

;; =============================================================================
;; Help System Commands
;; =============================================================================

(rf/reg-event-fx
 :describe-bindings
 (fn [{:keys [db]} [_]]
   "Display all current keybindings in a *Help* buffer (C-h b)"
   (let [buffers (:buffers db)
         keymaps (:keymaps db)
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
         active-buffer (get-in db [:buffers active-buffer-id])
         major-mode (:major-mode active-buffer :fundamental-mode)

         ;; Check if *Help* buffer already exists
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]
     (if help-buffer
       ;; Just switch to existing help buffer
       {:fx [[:dispatch [:switch-buffer (:id help-buffer)]]]}
       ;; Create new help buffer with bindings
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])

             ;; Format keybindings
             global-bindings (get-in keymaps [:global :bindings])
             major-bindings (get-in keymaps [:major major-mode :bindings])
             minor-bindings (get-in keymaps [:minor :bindings])

             format-bindings (fn [bindings title]
                              (when (seq bindings)
                                (str title ":\n"
                                     (clojure.string/join "\n"
                                       (map (fn [[key cmd]]
                                             (str "  " key "\t\t" (name cmd)))
                                            (sort-by first bindings)))
                                     "\n\n")))

             header "Key Bindings\n============\n\n"
             major-section (when (seq major-bindings)
                            (format-bindings major-bindings
                                            (str (name major-mode) " Mode")))
             global-section (format-bindings global-bindings "Global")

             content (str header
                         (or major-section "")
                         global-section)
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmGapBuffer. content)]

         (let [new-buffer {:id buffer-id
                          :wasm-instance wasm-instance
                          :file-handle nil
                          :name "*Help*"
                          :is-modified? false
                          :mark-position nil
                          :cursor-position {:line 0 :column 0}
                          :selection-range nil
                          :major-mode :help-mode
                          :minor-modes #{}
                          :buffer-local-vars {}
                          :ast nil
                          :language :text
                          :diagnostics []
                          :undo-stack []
                          :undo-in-progress? false
                          :editor-version 0
                          :cache {:text content
                                  :line-count line-count}}]
           {:db (-> db
                    (assoc-in [:buffers buffer-id] new-buffer)
 )
           :fx [[:dispatch [:switch-buffer buffer-id]]]}))))))

(rf/reg-event-fx
 :describe-key
 (fn [{:keys [db]} [_]]
   "Describe what a key does (C-h k)"
   {:db (-> db
            (assoc-in [:help :awaiting-key?] true)
            (assoc-in [:help :callback] [:describe-key/show]))
    :fx [[:dispatch [:echo/message "Describe key: "]]]}))

(rf/reg-event-fx
 :describe-key/show
 (fn [{:keys [db]} [_ key-str]]
   "Show description of a key binding"
   (let [keymaps (:keymaps db)
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer (get-in db [:buffers (:buffer-id active-window)])
         major-mode (:major-mode active-buffer :fundamental-mode)

         ;; Look up key in keymaps (major mode first, then global)
         major-bindings (get-in keymaps [:major major-mode])
         global-bindings (get-in keymaps [:global])
         command (or (get major-bindings key-str)
                    (get global-bindings key-str))

         ;; Get command docstring from registry
         command-def (get-in db [:commands command])
         docstring (or (:docstring command-def) "No documentation available")

         ;; Format help text
         content (if command
                  (str key-str " runs the command " (name command) "\n\n"
                       docstring)
                  (str key-str " is undefined"))

         buffers (:buffers db)
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]

     (if help-buffer
       ;; Update existing help buffer
       (let [buffer-id (:id help-buffer)
             ^js wasm-instance (:wasm-instance help-buffer)
             current-length (.length wasm-instance)
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.delete wasm-instance 0 current-length)
         (.insert wasm-instance 0 content)
         {:db (-> db
                  (assoc-in [:help :awaiting-key?] false)
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]
               [:dispatch [:echo/clear]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmGapBuffer. content)
             new-buffer {:id buffer-id
                        :wasm-instance wasm-instance
                        :file-handle nil
                        :name "*Help*"
                        :is-modified? false
                        :major-mode :help-mode
                        :minor-modes #{}
                        :buffer-local-vars {}
                        :ast nil
                        :language :text
                        :diagnostics []
                        :undo-stack []
                        :undo-in-progress? false
                        :editor-version 0
                        :cache {:text content
                                :line-count line-count}}]
         {:db (-> db
                  (assoc-in [:help :awaiting-key?] false)
                  (assoc-in [:buffers buffer-id] new-buffer)
 )
         :fx [[:dispatch [:switch-buffer buffer-id]]
              [:dispatch [:echo/clear]]]})))))

(rf/reg-event-fx
 :describe-function
 (fn [{:keys [db]} [_]]
   "Describe a function/command (C-h f)"
   (let [commands (:commands db)
         command-names (sort (map name (keys commands)))]
     {:fx [[:dispatch [:minibuffer/activate
                      {:prompt "Describe function: "
                       :on-confirm [:describe-function/show]
                       :completions command-names}]]]})))

(rf/reg-event-fx
 :describe-function/show
 (fn [{:keys [db]} [_ function-name]]
   "Show description of a function"
   (let [command-keyword (keyword function-name)
         command-def (get-in db [:commands command-keyword])
         docstring (or (:docstring command-def) "No documentation available")

         ;; Find all keybindings for this command
         keymaps (:keymaps db)
         global-bindings (get-in keymaps [:global])
         major-bindings (get-in keymaps [:major])

         find-keys (fn [bindings]
                    (keep (fn [[k v]] (when (= v command-keyword) k))
                          bindings))

         global-keys (find-keys global-bindings)
         major-keys (mapcat (fn [[mode bindings]]
                             (map #(str % " (" (name mode) ")")
                                  (find-keys bindings)))
                           major-bindings)

         all-keys (concat global-keys major-keys)
         key-section (if (seq all-keys)
                      (str "\n\nKey bindings:\n  "
                           (clojure.string/join "\n  " all-keys))
                      "")

         content (if command-def
                  (str function-name " is an interactive command\n\n"
                       docstring
                       key-section)
                  (str function-name " is not defined"))

         buffers (:buffers db)
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]

     (if help-buffer
       ;; Update existing help buffer
       (let [buffer-id (:id help-buffer)
             ^js wasm-instance (:wasm-instance help-buffer)
             current-length (.length wasm-instance)
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.delete wasm-instance 0 current-length)
         (.insert wasm-instance 0 content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmGapBuffer. content)
             new-buffer {:id buffer-id
                        :wasm-instance wasm-instance
                        :file-handle nil
                        :name "*Help*"
                        :is-modified? false
                        :major-mode :help-mode
                        :minor-modes #{}
                        :buffer-local-vars {}
                        :ast nil
                        :language :text
                        :diagnostics []
                        :undo-stack []
                        :undo-in-progress? false
                        :editor-version 0
                        :cache {:text content
                                :line-count line-count}}]
         {:db (-> db
                  (assoc-in [:buffers buffer-id] new-buffer))
         :fx [[:dispatch [:switch-buffer buffer-id]]]}))

)))

(rf/reg-event-fx
 :describe-variable
 (fn [{:keys [db]} [_]]
   "Describe a variable (C-h v)"
   ;; For now, just show buffer-local variables and some common ones
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer (get-in db [:buffers (:buffer-id active-window)])
         buffer-local-vars (keys (:buffer-local-vars active-buffer))
         common-vars ["kill-ring" "prefix-argument" "mark-position"]
         all-vars (sort (concat (map name buffer-local-vars) common-vars))]
     {:fx [[:dispatch [:minibuffer/activate
                      {:prompt "Describe variable: "
                       :on-confirm [:describe-variable/show]
                       :completions all-vars}]]]})))

(rf/reg-event-fx
 :describe-variable/show
 (fn [{:keys [db]} [_ var-name]]
   "Show description of a variable"
   (let [var-keyword (keyword var-name)
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer (get-in db [:buffers (:buffer-id active-window)])

         ;; Try to find variable value
         var-value (case var-name
                    "kill-ring" (pr-str (take 3 (:kill-ring db)))
                    "prefix-argument" (pr-str (get-in db [:ui :prefix-argument]))
                    "mark-position" (pr-str (get-in active-window [:mark-position]))
                    (or (get-in active-buffer [:buffer-local-vars var-keyword])
                        "undefined"))

         content (str var-name " is a variable\n\n"
                     "Value: " var-value "\n\n"
                     "Documentation:\n"
                     (case var-name
                       "kill-ring" "List of killed text strings"
                       "prefix-argument" "Numeric argument for commands (set by C-u)"
                       "mark-position" "Position of the mark in current buffer"
                       "No documentation available"))

         buffers (:buffers db)
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]

     (if help-buffer
       ;; Update existing help buffer
       (let [buffer-id (:id help-buffer)
             ^js wasm-instance (:wasm-instance help-buffer)
             current-length (.length wasm-instance)
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.delete wasm-instance 0 current-length)
         (.insert wasm-instance 0 content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmGapBuffer. content)
             new-buffer {:id buffer-id
                        :wasm-instance wasm-instance
                        :file-handle nil
                        :name "*Help*"
                        :is-modified? false
                        :major-mode :help-mode
                        :minor-modes #{}
                        :buffer-local-vars {}
                        :ast nil
                        :language :text
                        :diagnostics []
                        :undo-stack []
                        :undo-in-progress? false
                        :editor-version 0
                        :cache {:text content
                                :line-count line-count}}]
         {:db (-> db
                  (assoc-in [:buffers buffer-id] new-buffer)
 )
         :fx [[:dispatch [:switch-buffer buffer-id]]]})))))

(rf/reg-event-fx
 :apropos-command
 (fn [{:keys [db]} [_]]
   "Search for commands matching a pattern (C-h a)"
   {:fx [[:dispatch [:minibuffer/activate
                    {:prompt "Apropos command (regex): "
                     :on-confirm [:apropos-command/show]}]]]}))

(rf/reg-event-fx
 :apropos-command/show
 (fn [{:keys [db]} [_ pattern]]
   "Show commands matching a pattern"
   (let [commands (:commands db)
         regex (try (re-pattern (str "(?i)" pattern))
                   (catch js/Error _ nil))

         matches (if regex
                  (filter (fn [[cmd-name cmd-def]]
                           (or (re-find regex (name cmd-name))
                               (re-find regex (or (:docstring cmd-def) ""))))
                         commands)
                  [])

         content (if (seq matches)
                  (str "Commands matching \"" pattern "\":\n\n"
                       (clojure.string/join "\n"
                         (map (fn [[cmd-name cmd-def]]
                               (str (name cmd-name) "\n  "
                                    (or (:docstring cmd-def) "No documentation")
                                    "\n"))
                             (sort-by first matches))))
                  (str "No commands match \"" pattern "\""))

         buffers (:buffers db)
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]

     (if help-buffer
       ;; Update existing help buffer
       (let [buffer-id (:id help-buffer)
             ^js wasm-instance (:wasm-instance help-buffer)
             current-length (.length wasm-instance)
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.delete wasm-instance 0 current-length)
         (.insert wasm-instance 0 content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmGapBuffer. content)
             new-buffer {:id buffer-id
                        :wasm-instance wasm-instance
                        :file-handle nil
                        :name "*Help*"
                        :is-modified? false
                        :major-mode :help-mode
                        :minor-modes #{}
                        :buffer-local-vars {}
                        :ast nil
                        :language :text
                        :diagnostics []
                        :undo-stack []
                        :undo-in-progress? false
                        :editor-version 0
                        :cache {:text content
                                :line-count line-count}}]
         {:db (-> db
                  (assoc-in [:buffers buffer-id] new-buffer))
         :fx [[:dispatch [:switch-buffer buffer-id]]]})))))

(rf/reg-event-fx
 :help-for-help
 (fn [{:keys [db]} [_]]
   "Show help menu (C-h ?)"
   (let [content "Help Commands:

C-h k   Describe key: show what a key does
C-h f   Describe function: show command documentation
C-h b   Describe bindings: list all keybindings
C-h a   Apropos command: search commands by pattern
C-h ?   This help menu
"

         buffers (:buffers db)
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]

     (if help-buffer
       ;; Update existing help buffer
       (let [buffer-id (:id help-buffer)
             ^js wasm-instance (:wasm-instance help-buffer)
             current-length (.length wasm-instance)
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.delete wasm-instance 0 current-length)
         (.insert wasm-instance 0 content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmGapBuffer. content)
             new-buffer {:id buffer-id
                        :wasm-instance wasm-instance
                        :file-handle nil
                        :name "*Help*"
                        :is-modified? false
                        :major-mode :help-mode
                        :minor-modes #{}
                        :buffer-local-vars {}
                        :ast nil
                        :language :text
                        :diagnostics []
                        :undo-stack []
                        :undo-in-progress? false
                        :editor-version 0
                        :cache {:text content
                                :line-count line-count}}]
         {:db (-> db
                  (assoc-in [:buffers buffer-id] new-buffer))
         :fx [[:dispatch [:switch-buffer buffer-id]]]})))))

;; =============================================================================
;; Mode Initialization Commands
;; =============================================================================

(rf/reg-event-fx
 :initialize-mode-commands
 (fn [{:keys [db]} [_]]
   "Initialize mode-related commands"
   {:fx [[:dispatch [:register-command :fundamental-mode
                    {:docstring "Switch to fundamental mode"
                     :handler [:set-major-mode :fundamental-mode]}]]
         [:dispatch [:register-command :text-mode
                    {:docstring "Switch to text mode"
                     :handler [:set-major-mode :text-mode]}]]
         [:dispatch [:register-command :clojure-mode
                    {:docstring "Switch to Clojure mode"
                     :handler [:set-major-mode :clojure-mode]}]]
         [:dispatch [:register-command :line-number-mode
                    {:docstring "Toggle line number display"
                     :handler [:toggle-minor-mode :line-number-mode]}]]
         [:dispatch [:register-command :column-number-mode
                    {:docstring "Toggle column number display"
                     :handler [:toggle-minor-mode :column-number-mode]}]]]}))

;; =============================================================================
;; Command Initialization
;; =============================================================================

(rf/reg-event-fx
 :initialize-commands
 (fn [{:keys [db]} [_]]
   "Initialize built-in commands in the command registry"
   {:fx [[:dispatch [:register-command :find-file
                    {:docstring "Open a file"
                     :handler [:find-file]}]]
         [:dispatch [:register-command :save-buffer
                    {:docstring "Save current buffer"
                     :handler [:save-buffer]}]]
         [:dispatch [:register-command :write-file
                    {:docstring "Write buffer to file (save as)"
                     :handler [:write-file]}]]
         [:dispatch [:register-command :insert-file
                    {:docstring "Insert contents of file at point"
                     :handler [:insert-file]}]]
         [:dispatch [:register-command :revert-buffer
                    {:docstring "Revert buffer to saved file on disk"
                     :handler [:revert-buffer]}]]
         [:dispatch [:register-command :save-some-buffers
                    {:docstring "Offer to save modified buffers"
                     :handler [:save-some-buffers]}]]
         [:dispatch [:register-command :find-alternate-file
                    {:docstring "Kill current buffer and visit different file"
                     :handler [:find-alternate-file]}]]
         [:dispatch [:register-command :replace-string
                    {:docstring "Replace string non-interactively from point forward"
                     :handler [:replace-string]}]]
         [:dispatch [:register-command :replace-regexp
                    {:docstring "Replace regexp non-interactively from point forward"
                     :handler [:replace-regexp]}]]
         [:dispatch [:register-command :query-replace
                    {:docstring "Query and replace interactively (M-%)"
                     :handler [:query-replace]}]]
         [:dispatch [:register-command :query-replace-regexp
                    {:docstring "Query and replace regexp interactively (C-M-%)"
                     :handler [:query-replace-regexp]}]]
         [:dispatch [:register-command :isearch-forward
                    {:docstring "Incremental search forward (C-s)"
                     :handler [:isearch-forward]}]]
         [:dispatch [:register-command :isearch-backward
                    {:docstring "Incremental search backward (C-r)"
                     :handler [:isearch-backward]}]]
         [:dispatch [:register-command :switch-to-buffer
                    {:docstring "Switch to another buffer"
                     :handler [:switch-to-buffer]}]]
         [:dispatch [:register-command :kill-buffer
                    {:docstring "Kill (close) a buffer"
                     :handler [:kill-buffer]}]]
         [:dispatch [:register-command :list-buffers
                    {:docstring "Display a list of all buffers"
                     :handler [:list-buffers]}]]
         [:dispatch [:register-command :describe-bindings
                    {:docstring "Display all current keybindings"
                     :handler [:describe-bindings]}]]
         [:dispatch [:register-command :describe-key
                    {:docstring "Describe what a key does"
                     :handler [:describe-key]}]]
         [:dispatch [:register-command :describe-function
                    {:docstring "Describe a function/command"
                     :handler [:describe-function]}]]
         [:dispatch [:register-command :describe-variable
                    {:docstring "Describe a variable"
                     :handler [:describe-variable]}]]
         [:dispatch [:register-command :apropos-command
                    {:docstring "Search for commands matching a pattern"
                     :handler [:apropos-command]}]]
         [:dispatch [:register-command :help-for-help
                    {:docstring "Show help menu"
                     :handler [:help-for-help]}]]
         [:dispatch [:register-command :keyboard-quit
                    {:docstring "Cancel current operation"
                     :handler [:keyboard-quit]}]]
         [:dispatch [:register-command :universal-argument
                    {:docstring "Set or multiply universal argument (C-u)"
                     :handler [:universal-argument]}]]
         [:dispatch [:register-command :split-window-below
                    {:docstring "Split window horizontally"
                     :handler [:split-window-below]}]]
         [:dispatch [:register-command :split-window-right
                    {:docstring "Split window vertically"
                     :handler [:split-window-right]}]]
         [:dispatch [:register-command :delete-window
                    {:docstring "Delete current window"
                     :handler [:delete-window]}]]
         [:dispatch [:register-command :delete-other-windows
                    {:docstring "Delete all other windows"
                     :handler [:delete-other-windows]}]]
         [:dispatch [:register-command :other-window
                    {:docstring "Switch to next window"
                     :handler [:other-window]}]]
         [:dispatch [:register-command :undo
                    {:docstring "Undo last change"
                     :handler [:undo]}]]
         [:dispatch [:register-command :kill-region
                    {:docstring "Kill (cut) the active region"
                     :handler [:kill-region]}]]
         [:dispatch [:register-command :yank
                    {:docstring "Yank (paste) from kill ring"
                     :handler [:yank]}]]
         [:dispatch [:register-command :yank-pop
                    {:docstring "Replace last yank with next kill ring item"
                     :handler [:yank-pop]}]]
         [:dispatch [:register-command :kill-line
                    {:docstring "Kill from cursor to end of line"
                     :handler [:kill-line]}]]
         [:dispatch [:register-command :kill-word
                    {:docstring "Kill from cursor to end of word (M-d)"
                     :handler [:kill-word]}]]
         [:dispatch [:register-command :backward-kill-word
                    {:docstring "Kill from cursor to beginning of word (M-DEL)"
                     :handler [:backward-kill-word]}]]
         [:dispatch [:register-command :open-line
                    {:docstring "Insert newline without moving cursor"
                     :handler [:open-line]}]]
         [:dispatch [:register-command :set-mark-command
                    {:docstring "Set mark at current position"
                     :handler [:set-mark-command]}]]
         [:dispatch [:register-command :exchange-point-and-mark
                    {:docstring "Exchange point and mark (C-x C-x)"
                     :handler [:exchange-point-and-mark]}]]
         [:dispatch [:register-command :copy-region-as-kill
                    {:docstring "Copy region to kill ring"
                     :handler [:copy-region-as-kill]}]]
         [:dispatch [:register-command :delete-backward-char
                    {:docstring "Delete character before cursor"
                     :handler [:delete-backward-char]}]]
         [:dispatch [:register-command :delete-forward-char
                    {:docstring "Delete character after cursor"
                     :handler [:delete-forward-char]}]]
         [:dispatch [:register-command :forward-char
                    {:docstring "Move cursor forward one character"
                     :handler [:forward-char]}]]
         [:dispatch [:register-command :backward-char
                    {:docstring "Move cursor backward one character"
                     :handler [:backward-char]}]]
         [:dispatch [:register-command :next-line
                    {:docstring "Move cursor to next line"
                     :handler [:next-line]}]]
         [:dispatch [:register-command :previous-line
                    {:docstring "Move cursor to previous line"
                     :handler [:previous-line]}]]
         [:dispatch [:register-command :beginning-of-line
                    {:docstring "Move cursor to beginning of line"
                     :handler [:beginning-of-line]}]]
         [:dispatch [:register-command :end-of-line
                    {:docstring "Move cursor to end of line"
                     :handler [:end-of-line]}]]
         [:dispatch [:register-command :beginning-of-buffer
                    {:docstring "Move cursor to beginning of buffer"
                     :handler [:beginning-of-buffer]}]]
         [:dispatch [:register-command :end-of-buffer
                    {:docstring "Move cursor to end of buffer"
                     :handler [:end-of-buffer]}]]
         [:dispatch [:register-command :forward-word
                    {:docstring "Move cursor forward one word"
                     :handler [:forward-word]}]]
         [:dispatch [:register-command :backward-word
                    {:docstring "Move cursor backward one word"
                     :handler [:backward-word]}]]
         [:dispatch [:register-command :scroll-up-command
                    {:docstring "Scroll forward one screen (C-v)"
                     :handler [:scroll-up-command]}]]
         [:dispatch [:register-command :scroll-down-command
                    {:docstring "Scroll backward one screen (M-v)"
                     :handler [:scroll-down-command]}]]
         [:dispatch [:register-command :execute-extended-command
                    {:docstring "Execute extended command (M-x)"
                     :handler [:execute-extended-command]}]]
         ;; Initialize mode commands
         [:dispatch [:initialize-mode-commands]]
         ;; Update save-buffer to use hooks
         [:dispatch [:update-save-buffer-command]]]}))

;; =============================================================================
;; Legacy Command Registry (for backward compatibility)
;; =============================================================================

;; Registry for command functions
(defonce command-registry (atom {}))

(defn register-command!
  "Register a command function in the command registry"
  [command-key command-fn]
  (swap! command-registry assoc command-key command-fn))

(defn get-command
  "Get a command function from the registry"
  [command-key]
  (get @command-registry command-key))

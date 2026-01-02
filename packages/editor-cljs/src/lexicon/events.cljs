(ns lexicon.events
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.db :as db]
            [lexicon.cache :as cache]
            [lexicon.constants :as const]
            [lexicon.packages :as packages]
            [lexicon.completion.styles :as completion-styles]
            [lexicon.completion.metadata :as completion-metadata]
            [re-frame.std-interceptors :refer [debug]]
            ;; Event modules (auto-register handlers)
            [lexicon.events.keymap]
            [lexicon.events.mode]
            [lexicon.events.wasm]
            [lexicon.events.buffer :as buffer-events]
            [lexicon.events.edit]))

;; -- Helper Functions --

;; Helper functions moved to lexicon.events.buffer:
;; - line-col-to-linear-pos
;; - detect-language-from-filename

;; Helper functions moved to lexicon.events.edit:
;; - word-char?
;; - find-forward-word-boundary
;; - find-backward-word-boundary
;; - linear-pos-to-line-col

;; -- Initialization Events --

(rf/reg-event-fx
 :initialize-db
 (fn [_ _]
   "Initialize the application database with default state"
   {:db db/default-db
    :fx [[:dispatch [:initialize-commands]]]}))

;; WASM and parser event handlers moved to lexicon.events.wasm
;; - :wasm-module-loaded
;; - :wasm-load-failed

;; =============================================================================
;; REMOVED: FSM State Management (Evil-mode)
;; =============================================================================
;; All FSM (Finite State Machine) code for modal editing has been moved to:
;; packages/evil-mode/src/lexicon/evil/fsm/
;;
;; This includes:
;; - State lifecycle hooks (enter/exit)
;; - Interceptors for state transitions
;; - FSM transition events (:fsm/transition-to, etc.)
;; - Operator-pending state management
;;
;; Evil-mode will be re-implemented as an external package in Phase 6.
;; Core Emacs does not have modal editing - it uses standard Emacs keybindings.
;; =============================================================================

;; -- Emacs-style Command Registry and Execution --

(rf/reg-event-db
 :register-command
 (fn [db [_ command-name command-definition]]
   "Register a new command in the central command registry"
   (assoc-in db [:commands command-name] command-definition)))

(rf/reg-event-fx
 :execute-command
 (fn [{:keys [db]} [_ command-name & args]]
   "Execute a command by name from the central registry"
   (let [command-def (get-in db [:commands command-name])]
     (if command-def
       (let [handler (:handler command-def)
             should-clear-prefix? (not= command-name :universal-argument)]
         {:fx (cond-> [[:dispatch (into handler args)]]
                should-clear-prefix? (conj [:dispatch [:clear-prefix-argument]]))})
       {:fx [[:dispatch [:show-error (str "Command not found: " command-name)]]]}))))

(rf/reg-event-db
 :show-error
 (fn [db [_ message]]
   "Show an error message to the user"
   (println "Error:" message)
   (assoc-in db [:system :last-error] message)))

(rf/reg-event-fx
 :keyboard-quit
 (fn [{:keys [db]} [_]]
   "Cancel the current operation (equivalent to C-g in Emacs)"
   (let [minibuffer-active? (get-in db [:minibuffer :active?])
         on-cancel (get-in db [:minibuffer :on-cancel] [:minibuffer/deactivate])]
     (if minibuffer-active?
       ;; If minibuffer is active, cancel it and dispatch on-cancel
       {:db (-> db
                (assoc-in [:ui :selection] {:start 0 :end 0})
                (assoc-in [:fsm :operator-pending] nil))
        :fx [[:dispatch on-cancel]
             [:dispatch [:echo/message "Quit"]]]}
       ;; Otherwise just clear pending operations and show quit message
       {:db (-> db
                (assoc-in [:ui :selection] {:start 0 :end 0})
                (assoc-in [:fsm :operator-pending] nil))
        :fx [[:dispatch [:echo/message "Quit"]]]}))))

;; -- Key Sequence Parsing and Processing --

(defn key-event-to-string
  "Convert a KeyboardEvent to Emacs-style key string notation"
  [event]
  (let [ctrl? (.-ctrlKey event)
        meta? (.-metaKey event)
        alt? (.-altKey event)
        shift? (.-shiftKey event)
        key (.-key event)
        code (.-code event)]
    (cond
      ;; Special handling for certain keys
      (= key "Control") nil  ; Ignore standalone modifier keys
      (= key "Meta") nil
      (= key "Alt") nil
      (= key "Shift") nil

      ;; Handle special keys (without modifiers)
      (and (= key "Escape") (not ctrl?) (not meta?) (not alt?)) "ESC"
      (and (= key "Enter") (not ctrl?) (not meta?) (not alt?)) "RET"
      (and (= key "Tab") (not ctrl?) (not meta?) (not alt?)) "TAB"
      (and (= key "Backspace") (not ctrl?) (not meta?) (not alt?)) "DEL"
      (and (= key "Delete") (not ctrl?) (not meta?) (not alt?)) "DELETE"
      (and (= key " ") (not ctrl?) (not meta?) (not alt?)) "SPC"

      ;; Function keys
      (and (>= (.indexOf key "F") 0)
           (js/isNaN (js/parseInt (subs key 1))))
      key

      ;; Regular keys with modifiers (including special keys with modifiers)
      :else
      (let [base-key (cond
                       (= key " ") "SPC"
                       (= key "Enter") "RET"
                       (= key "Tab") "TAB"
                       (= key "Backspace") "DEL"
                       (= key "Delete") "DELETE"
                       (= key "Escape") "ESC"
                       (and shift? (= (count key) 1)) (.toUpperCase key)
                       :else key)]
        (str
         (when ctrl? "C-")
         (when (or meta? alt?) "M-")  ; Map both Meta and Alt keys to M- (Emacs convention)
         base-key)))))

(defn parse-key-sequence
  "Parse a key sequence string like 'C-x C-f' into a vector of individual keys"
  [key-sequence-str]
  (when key-sequence-str
    (clojure.string/split (clojure.string/trim key-sequence-str) #"\s+")))

(defn normalize-key-sequence
  "Normalize a key sequence vector to handle common variations"
  [key-sequence]
  (mapv (fn [key]
          (case key
            "C-m" "RET"     ; Ctrl+M is Enter
            "C-i" "TAB"     ; Ctrl+I is Tab
            "C-[" "ESC"     ; Ctrl+[ is Escape
            key))
        key-sequence))

;; Keymap event handlers moved to lexicon.events.keymap
;; - :set-prefix-key-state
;; - :clear-prefix-key-state
;; - :handle-key-sequence

;; Mode event handlers moved to lexicon.events.mode
;; - :set-major-mode
;; - :toggle-minor-mode
;; - :enable-minor-mode
;; - :disable-minor-mode
;; - :line-number-mode
;; - :column-number-mode

;; -- Package Management Commands (Phase 6) --

(rf/reg-event-fx
 :load-package
 (fn [{:keys [db]} [_ package-name]]
   "Load a package by name"
   (packages/load-package! package-name)
   {:db db}))

(rf/reg-event-fx
 :unload-package
 (fn [{:keys [db]} [_ package-name]]
   "Unload a package by name"
   (packages/unload-package! package-name)
   {:db db}))

;; Register mode-related commands
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
                     :handler [:toggle-minor-mode :line-number-mode]}]]]}))

;; Text editing event handlers moved to lexicon.events.edit
;; - :delete-backward-char
;; - :delete-forward-char

;; Cursor movement event handlers moved to lexicon.events.edit
;; - :forward-char
;; - :backward-char
;; - :next-line
;; - :previous-line
;; - :click-to-position
;; - :beginning-of-line
;; - :end-of-line
;; - :beginning-of-buffer
;; - :end-of-buffer
;; - :forward-word
;; - :backward-word

;; Editing event handlers moved to lexicon.events.edit
;; - :kill-line
;; - :open-line

;; Mark and region event handlers moved to lexicon.events.edit
;; - :set-mark-command
;; - :set-mark
;; - :copy-region-as-kill
;; - :kill-region

;; -- Universal Argument (Prefix Argument) --

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

;; Cursor management event handlers moved to lexicon.events.edit
;; - :initialize-buffer-cursor
;; - :update-cursor-position
;; - :set-cursor-position


;; Update the main initialization to include mode commands
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
         [:dispatch [:register-command :open-line
                    {:docstring "Insert newline without moving cursor"
                     :handler [:open-line]}]]
         [:dispatch [:register-command :set-mark-command
                    {:docstring "Set mark at current position"
                     :handler [:set-mark-command]}]]
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
         [:dispatch [:register-command :execute-extended-command
                    {:docstring "Execute extended command (M-x)"
                     :handler [:execute-extended-command]}]]
         ;; Initialize mode commands
         [:dispatch [:initialize-mode-commands]]
         ;; Update save-buffer to use hooks
         [:dispatch [:update-save-buffer-command]]]}))

;; -- Hook System Implementation --

;; Hook event handlers moved to lexicon.events.mode
;; - :add-hook
;; - :remove-hook
;; - :run-hook

;; Example hook usage - update save-buffer to use before-save-hook
(rf/reg-event-fx
 :save-buffer-with-hooks
 (fn [{:keys [db]} [_]]
   "Save buffer with before-save and after-save hooks"
   {:fx [[:dispatch [:run-hook :before-save-hook]]
         [:dispatch [:save-buffer-internal]]
         [:dispatch [:run-hook :after-save-hook]]]}))

;; Create internal save-buffer event (renamed from original)
(rf/reg-event-fx
 :save-buffer-internal
 (fn [{:keys [db]} [_]]
   "Internal save buffer implementation"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         file-handle (:file-handle active-buffer)]
     
     (if wasm-instance
       (let [content (.getText ^js wasm-instance)]
         (if file-handle
           ;; Save to existing file
           {:fx [[:save-to-file-handle {:file-handle file-handle
                                        :content content
                                        :buffer-id active-buffer-id}]]}
           ;; Prompt for save location
           {:fx [[:save-file-picker {:content content
                                     :buffer-id active-buffer-id}]]}))
       {:db db}))))

;; Update the original save-buffer command registration to use hooks
(rf/reg-event-fx
 :update-save-buffer-command
 (fn [{:keys [db]} [_]]
   "Update save-buffer command to use hook system"
   {:fx [[:dispatch [:register-command :save-buffer 
                    {:docstring "Save current buffer with hooks"
                     :handler [:save-buffer-with-hooks]}]]]}))

;; -- Legacy Command Registry (for backward compatibility) --

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

;; REMOVED: Evil-mode example commands
;; These have been moved to packages/evil-mode/
;; Evil-mode will be implemented as an external package in Phase 6

;; =============================================================================
;; REMOVED: Modal Command Dispatcher (Evil-mode)
;; =============================================================================
;; The modal/FSM-aware command dispatcher has been moved to:
;; packages/evil-mode/src/lexicon/evil/command/dispatcher.cljs
;;
;; This included:
;; - :modal/dispatch-key event (FSM-aware key dispatcher)
;; - Operator-motion composition logic
;; - State transition commands (:modal/enter-insert-mode, etc.)
;; - Mode-specific command registration
;;
;; Core Emacs uses a simple keymap lookup → command execution model.
;; No FSM, no modal states, no operator-pending logic.
;; =============================================================================

;; Buffer event handlers moved to lexicon.events.buffer
;; - :create-buffer
;; - :switch-buffer
;; - :switch-to-buffer
;; - :switch-to-buffer-by-name
;; - :kill-buffer
;; - :kill-buffer-by-name
;; - :list-buffers
;; - :buffer-menu/select-buffer
;; - :save-buffer
;; - :write-file
;; - :buffer-saved
;; - :find-file
;; - :file-read-success
;; - :file-read-failure
;; - :close-buffer
;; - :save-file-picker (effect)
;; - :save-to-file-handle (effect)
;; - :open-file-picker (effect)

(rf/reg-event-fx
 :describe-bindings
 (fn [{:keys [db]} [_]]
   "Display all current keybindings in a *Help* buffer (C-h b)"
   (let [buffers (:buffers db)
         keymaps (:keymaps db)
         active-buffer-id (get-in db [:windows (:active-window-id db) :buffer-id])
         active-buffer (get-in db [:buffers active-buffer-id])
         major-mode (:major-mode active-buffer :fundamental-mode)

         ;; Check if *Help* buffer already exists
         help-buffer (first (filter #(= (:name %) "*Help*") (vals buffers)))]
     (if help-buffer
       ;; Just switch to existing help buffer
       {:fx [[:dispatch [:switch-buffer (:id help-buffer)]]]}
       ;; Create new help buffer with bindings
       (let [buffer-id (db/next-buffer-id buffers)
             WasmEditorCore (get-in db [:system :wasm-constructor])

             ;; Format keybindings
             global-bindings (get-in keymaps [:global])
             major-bindings (get-in keymaps [:major major-mode])
             minor-bindings (get-in keymaps [:minor])

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
             wasm-instance (WasmEditorCore. content)]

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
                    (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))))))

;; -- Help System Commands (Phase 4) --

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
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.setText wasm-instance content)
         {:db (-> db
                  (assoc-in [:help :awaiting-key?] false)
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]
               [:dispatch [:echo/clear]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmEditorCore (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmEditorCore. content)
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
                  (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))
          :fx [[:dispatch [:echo/clear]]]})))))

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
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.setText wasm-instance content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmEditorCore (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmEditorCore. content)
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
                  (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))

)))

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
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.setText wasm-instance content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmEditorCore (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmEditorCore. content)
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
                  (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))

)))

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
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)]
         (.setText wasm-instance content)
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:buffers buffer-id :editor-version] inc))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new help buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmEditorCore (get-in db [:system :wasm-constructor])
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmEditorCore. content)
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
                  (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))

)))

;; -- Window Management (Phase 3) --

(rf/reg-event-fx
 :split-window-below
 (fn [{:keys [db]} [_]]
   "Split current window horizontally (C-x 2)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)

         ;; Create new window with same buffer
         new-window-id (db/next-window-id db)
         new-window (db/create-leaf-window new-window-id (:buffer-id active-window))

         ;; Create split node
         split-id (inc new-window-id)
         split-node (db/create-split-window :hsplit split-id active-window new-window)

         ;; Function to replace the active window with the split
         replace-window (fn replace-window [tree]
                         (if (= (:id tree) active-window-id)
                           split-node
                           (cond
                             (= (:type tree) :leaf) tree
                             (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                             (assoc tree
                                    :first (replace-window (:first tree))
                                    :second (replace-window (:second tree)))
                             :else tree)))

         new-tree (replace-window window-tree)]

     {:db (-> db
              (assoc :window-tree new-tree)
              (assoc :active-window-id new-window-id)
              (assoc :next-window-id (+ new-window-id 2)))})))

(rf/reg-event-fx
 :split-window-right
 (fn [{:keys [db]} [_]]
   "Split current window vertically (C-x 3)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)

         ;; Create new window with same buffer
         new-window-id (db/next-window-id db)
         new-window (db/create-leaf-window new-window-id (:buffer-id active-window))

         ;; Create split node (vertical this time)
         split-id (inc new-window-id)
         split-node (db/create-split-window :vsplit split-id active-window new-window)

         ;; Function to replace the active window with the split
         replace-window (fn replace-window [tree]
                         (if (= (:id tree) active-window-id)
                           split-node
                           (cond
                             (= (:type tree) :leaf) tree
                             (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                             (assoc tree
                                    :first (replace-window (:first tree))
                                    :second (replace-window (:second tree)))
                             :else tree)))

         new-tree (replace-window window-tree)]

     {:db (-> db
              (assoc :window-tree new-tree)
              (assoc :active-window-id new-window-id)
              (assoc :next-window-id (+ new-window-id 2)))})))

(rf/reg-event-fx
 :other-window
 (fn [{:keys [db]} [_]]
   "Switch to next window (C-x o)"
   (let [window-tree (:window-tree db)
         all-windows (db/get-all-leaf-windows window-tree)
         active-window-id (:active-window-id db)

         ;; Find current window index
         current-index (first (keep-indexed
                               (fn [idx window]
                                 (when (= (:id window) active-window-id) idx))
                               all-windows))

         ;; Get next window (wrap around)
         next-index (mod (inc (or current-index 0)) (count all-windows))
         next-window (nth all-windows next-index)
         next-window-id (:id next-window)]

     {:db (assoc db :active-window-id next-window-id)})))

(rf/reg-event-db
 :set-active-window
 (fn [db [_ window-id]]
   "Set the active window by ID (used when clicking on a window)"
   (assoc db :active-window-id window-id)))

(rf/reg-event-fx
 :delete-window
 (fn [{:keys [db]} [_]]
   "Delete current window (C-x 0)"
   (let [window-tree (:window-tree db)
         all-windows (db/get-all-leaf-windows window-tree)
         active-window-id (:active-window-id db)]

     (if (<= (count all-windows) 1)
       ;; Can't delete the last window
       {:db db}
       ;; TODO: Implement window deletion logic
       ;; This requires removing the window from the tree and rebalancing
       {:fx [[:dispatch [:echo/message "delete-window not yet implemented"]]]}))))

(rf/reg-event-fx
 :delete-other-windows
 (fn [{:keys [db]} [_]]
   "Delete all windows except current (C-x 1)"
   (let [window-tree (:window-tree db)
         active-window-id (:active-window-id db)
         active-window (db/find-window-in-tree window-tree active-window-id)]

     {:db (assoc db :window-tree active-window)})))

;; Transaction queue handlers moved to lexicon.events.wasm
;; - :editor/set-flight-status
;; - :editor/queue-transaction
;; - :handle-text-input
;; - :editor/process-queue (effect)
;; - :editor/transaction-success
;; - :editor/transaction-failure
;; - :dispatch-transaction
;; - :apply-transaction-result
;; - :transaction-failed
;; - :compound-transaction


;; -- UI State Events --

;; :set-cursor-position moved to lexicon.events.edit

(rf/reg-event-db
 :set-selection
 (fn [db [_ start end]]
   "Update selection range"
   (assoc-in db [:ui :selection] {:start start :end end})))

(rf/reg-event-db
 :ime-composition-start
 (fn [db [_]]
   "Start IME composition"
   (assoc-in db [:ui :ime-composing?] true)))

(rf/reg-event-db
 :ime-composition-update
 (fn [db [_ text]]
   "Update IME composition text"
   (assoc-in db [:ui :ime-composition-text] text)))

(rf/reg-event-db
 :ime-composition-end
 (fn [db [_ final-text]]
   "End IME composition and commit text"
   (-> db
       (assoc-in [:ui :ime-composing?] false)
       (assoc-in [:ui :ime-composition-text] ""))))

(rf/reg-event-fx
 :ime-commit-composition
 (fn [coeffects [_ text position]]
   "Commit IME composition as a transaction"
   (rf/dispatch [:editor/queue-transaction {:op :insert :text text}])
   coeffects))

;; -- View Reconciliation Events --

(rf/reg-event-db
 :view-updated
 (fn [db [_]]
   "Mark that view has been updated"
   (assoc-in db [:ui :view-needs-update?] false)))

(rf/reg-event-fx
 :reconcile-dom-state
 (fn [{:keys [db]} [_ dom-content expected-content]]
   "Reconcile DOM state with WASM state using diff algorithm"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     (when wasm-instance
       ;; TODO: Implement text diffing algorithm
       ;; For now, replace entire content
       (let [current-length (.length ^js wasm-instance)]
         (.delete ^js wasm-instance 0 current-length)
         (.insert ^js wasm-instance 0 dom-content)))
     
     {:db (assoc-in db [:ui :view-needs-update?] true)})))

;; -- System Events --

(rf/reg-event-db
 :set-mutation-observer
 (fn [db [_ observer]]
   "Store MutationObserver instance"
   (assoc-in db [:system :mutation-observer] observer)))

(rf/reg-event-db
 :set-reconciliation-active
 (fn [db [_ active?]]
   "Set reconciliation active flag"
   (assoc-in db [:system :reconciliation-active?] active?)))

;; -- File System Access Events --
;; -- Minibuffer Events --

(rf/reg-event-db
 :minibuffer/activate
 (fn [db [_ config]]
   "Activate the minibuffer with given configuration.
   Config keys:
   - :prompt - Prompt string
   - :on-confirm - Event to dispatch on RET
   - :on-cancel - Event to dispatch on C-g
   - :completions - List of completion candidates
   - :metadata - Completion metadata (Phase 6C)"
   (-> db
       (assoc-in [:minibuffer :active?] true)
       (assoc-in [:minibuffer :prompt] (:prompt config ""))
       (assoc-in [:minibuffer :input] "")
       (assoc-in [:minibuffer :on-confirm] (:on-confirm config))
       (assoc-in [:minibuffer :on-cancel] (or (:on-cancel config) [:minibuffer/deactivate]))
       (assoc-in [:minibuffer :completions] (or (:completions config) []))
       (assoc-in [:minibuffer :completion-index] 0)
       (assoc-in [:minibuffer :completion-metadata] (:metadata config)))))

(rf/reg-fx
 :focus-editor
 (fn [_]
   "Focus the hidden input element to enable keyboard input"
   (when-let [hidden-input (js/document.querySelector ".hidden-input")]
     (.focus hidden-input))))

(rf/reg-event-fx
 :minibuffer/deactivate
 (fn [{:keys [db]} [_]]
   "Deactivate the minibuffer and reset state, then focus editor"
   {:db (-> db
            (assoc-in [:minibuffer :active?] false)
            (assoc-in [:minibuffer :prompt] "")
            (assoc-in [:minibuffer :input] "")
            (assoc-in [:minibuffer :on-confirm] nil)
            (assoc-in [:minibuffer :on-cancel] [:minibuffer/deactivate]))
    :fx [[:focus-editor]]}))

(rf/reg-event-db
 :minibuffer/set-input
 (fn [db [_ input-text]]
   "Update the minibuffer input text"
   (assoc-in db [:minibuffer :input] input-text)))

(rf/reg-event-db
 :minibuffer/complete
 (fn [db [_]]
   "TAB completion in minibuffer using completion styles (Phase 6C)"
   (let [minibuffer (:minibuffer db)
         input (:input minibuffer)
         completions (:completions minibuffer)
         ;; Get effective styles for current completion
         styles (or (get-in db [:completion :styles]) [:basic :substring :flex])
         ;; Filter completions using styles
         matches (if (clojure.string/blank? input)
                  completions
                  (completion-styles/filter-candidates
                   input
                   completions
                   :styles-list styles))]
     (cond
       ;; No completions available
       (empty? completions)
       db

       ;; Single match - complete it
       (= (count matches) 1)
       (assoc-in db [:minibuffer :input] (first matches))

       ;; Multiple matches - find common prefix and complete to that
       (> (count matches) 1)
       (let [common-prefix (reduce (fn [prefix candidate]
                                    (loop [i 0]
                                      (if (and (< i (count prefix))
                                              (< i (count candidate))
                                              (= (nth prefix i) (nth candidate i)))
                                        (recur (inc i))
                                        (subs prefix 0 i))))
                                  (first matches)
                                  (rest matches))]
         (if (> (count common-prefix) (count input))
           ;; There's a longer common prefix, complete to it
           (assoc-in db [:minibuffer :input] common-prefix)
           ;; No longer prefix, show matches in echo area
           (-> db
               (assoc-in [:echo-area :message] (str "[" (clojure.string/join ", " matches) "]")))))

       ;; No matches
       :else
       db))))

;;; -- Echo Area Events --

(rf/reg-event-fx
 :echo/message
 (fn [{:keys [db]} [_ message]]
   "Display a message in the echo area (auto-clears after 3 seconds)"
   (let [old-timeout-id (get-in db [:echo-area :timeout-id])]
     ;; Clear previous timeout if any
     (when old-timeout-id
       (js/clearTimeout old-timeout-id))
     ;; Set new message and create new timeout
     (let [timeout-id (js/setTimeout
                        #(rf/dispatch [:echo/clear])
                        3000)]
       {:db (-> db
                (assoc-in [:echo-area :message] message)
                (assoc-in [:echo-area :timeout-id] timeout-id))}))))

(rf/reg-event-db
 :echo/clear
 (fn [db [_]]
   "Clear the echo area message"
   (let [timeout-id (get-in db [:echo-area :timeout-id])]
     (when timeout-id
       (js/clearTimeout timeout-id))
     (-> db
         (assoc-in [:echo-area :message] "")
         (assoc-in [:echo-area :timeout-id] nil)))))

(rf/reg-event-fx
 :minibuffer/confirm
 (fn [{:keys [db]} [_]]
   "Confirm minibuffer input and execute the configured action"
   (let [minibuffer (:minibuffer db)
         input (:input minibuffer)
         on-confirm (:on-confirm minibuffer)]
     (if on-confirm
       {:fx [[:dispatch (conj on-confirm input)]
             [:dispatch [:minibuffer/deactivate]]]}
       {:fx [[:dispatch [:minibuffer/deactivate]]]}))))

;; -- Parser Worker Events --


;; Parser event handlers moved to lexicon.events.wasm
;; - :parser/start-worker (effect)
;; - :parser/worker-created
;; - :parser/worker-ready
;; - :parser/worker-error
;; - :parser/ast-updated
;; - :parser/parse-error
;; - :parser/request-parse
;; - :parser/request-incremental-parse

;; -- WebSocket Bridge Communication --

(rf/reg-fx
 :ws/connect
 (fn [{:keys [url on-open on-message on-close on-error]}]
   "Create WebSocket connection to lexicon-bridge server with ticket authentication"
   (when (and url (not (get-in @re-frame.db/app-db [:bridge :ws])))
     (try
       ;; First, get a ticket from the HTTP server
       (-> (js/fetch "http://localhost:30304/api/ws-ticket" 
                     (clj->js {:method "POST"
                               :headers {"Content-Type" "application/json"}}))
           (.then (fn [response]
                    (if (.-ok response)
                      (.json response)
                      (throw (js/Error. (str "Failed to get ticket: " (.-status response)))))))
           (.then (fn [ticket-data]
                    (let [ticket (.-ticket ^js ticket-data)
                          ws-url (str url "?ticket=" ticket)
                          ws (js/WebSocket. ws-url)]
                      (println "🎫 :client Got WebSocket ticket:" (subs ticket 0 8) "...")
                      
                      ;; Store the WebSocket immediately to prevent duplicate connections
                      (rf/dispatch [:ws/connecting ws])
         
                      (set! (.-onopen ws) 
                            (fn [event]
                              (println "✅ :client WebSocket connected with valid ticket")
                              (rf/dispatch [:ws/opened event])))
                      
                      (set! (.-onmessage ws)
                            (fn [event]
                              (let [data (js/JSON.parse (.-data event))]
                                (println "📨 :client Received message:" (.-type data))
                                (rf/dispatch [:ws/message-received (js->clj data :keywordize-keys true)]))))
                      
                      (set! (.-onclose ws)
                            (fn [event]
                              (println "🔌 :client WebSocket closed. Code:" (.-code event) "Reason:" (.-reason event))
                              (rf/dispatch [:ws/closed {:code (.-code event) 
                                                        :reason (.-reason event)
                                                        :was-clean (.-wasClean event)}])))
                      
                      (set! (.-onerror ws)
                            (fn [event]
                              (println "❌ :client WebSocket error")
                              (rf/dispatch [:ws/error {:error "WebSocket connection error"}])))
                      )))
           (.catch (fn [error]
                     (println "❌ :client Failed to get WebSocket ticket:" error)
                     (rf/dispatch [:ws/error {:error (str "Ticket acquisition failed: " error)}]))))
       (catch js/Error error
         (println "❌ :client WebSocket creation error:" error)
         (rf/dispatch [:ws/error {:error (str "Failed to create WebSocket: " error)}]))))))

(rf/reg-fx
 :ws/send
 (fn [{:keys [message]}]
   "Send message over active WebSocket connection"
   (println "🌐 WS: send effect called with message type:" (:type message))
   (let [ws (get-in @re-frame.db/app-db [:bridge :ws])
         status (get-in @re-frame.db/app-db [:bridge :status])]
     (println "🌐 WS: Connection status:" status "WebSocket exists:" (boolean ws))
     (println "🌐 WS: WebSocket readyState:" (when ws (.-readyState ws)))
     (println "🌐 WS: WebSocket URL:" (when ws (.-url ws)))
     (if (and ws (= status :connected))
       (try
         (println "🌐 WS: Sending message:" message)
         (.send ws (js/JSON.stringify (clj->js message)))
         (println "🌐 WS: Message sent successfully to:" (when ws (.-url ws)))
         (catch js/Error error
           (println "❌ WS: Failed to send WebSocket message:" error)
           (rf/dispatch [:ws/error {:error (str "Send failed: " error)}])))
       (println "❌ WS: Cannot send message - WebSocket not connected. Status:" status)))))

;; WebSocket Event Handlers

(rf/reg-event-db
 :ws/connecting
 (fn [db [_ ws]]
   "Set WebSocket connection status to connecting"
   (-> db
       (assoc-in [:bridge :ws] ws)
       (assoc-in [:bridge :status] :connecting))))

(rf/reg-event-db
 :ws/opened
 (fn [db [_ event]]
   "Handle WebSocket connection opened"
   (println "✅ Connected to lexicon-bridge")
   (-> db
       (assoc-in [:bridge :status] :connected)
       (assoc-in [:bridge :retry-count] 0))))

(rf/reg-event-fx
 :ws/closed
 (fn [{:keys [db]} [_ {:keys [code reason was-clean]}]]
   "Handle WebSocket connection closed"
   (println "🔌 WebSocket connection closed. Code:" code "Reason:" reason "Clean:" was-clean)
   (let [retry-count (get-in db [:bridge :retry-count] 0)
         max-retries (get-in db [:bridge :max-retries] 5)]
     {:db (-> db
              (assoc-in [:bridge :ws] nil)
              (assoc-in [:bridge :status] :disconnected))
      :fx (if (< retry-count max-retries)
            [[:dispatch-later {:ms 2000 :dispatch [:ws/connect]}]]
            [])})))

(rf/reg-event-fx
 :ws/error
 (fn [{:keys [db]} [_ {:keys [error]}]]
   "Handle WebSocket error"
   (println "❌ WebSocket error:" error)
   (let [retry-count (get-in db [:bridge :retry-count] 0)
         max-retries (get-in db [:bridge :max-retries] 5)]
     {:db (-> db
              (assoc-in [:bridge :status] :disconnected)
              (update-in [:bridge :retry-count] inc))
      :fx (if (< retry-count max-retries)
            [[:dispatch-later {:ms 3000 :dispatch [:ws/connect]}]]
            [])})))

(rf/reg-event-fx
 :ws/connect
 (fn [{:keys [db]} [_]]
   "Initiate WebSocket connection to bridge with ticket authentication"
   (let [url (get-in db [:bridge :url] "ws://localhost:30303")]
     (println "🌉 :client Initiating WebSocket connection to:" url)
     {:fx [[:ws/connect {:url url}]]})))

(rf/reg-event-fx
 :ws/message-received
 (fn [{:keys [db]} [_ message]]
   "Handle incoming WebSocket message from bridge"
   (let [msg-type (:type message)]
     (case msg-type
       "lsp/started" 
       {:fx [[:dispatch [:lsp/server-started message]]]}
       
       "lsp/stopped"
       {:fx [[:dispatch [:lsp/server-stopped message]]]}
       
       "lsp/message"
       {:fx [[:dispatch [:lsp/message-received message]]]}
       
       "error"
       (do
         (println "Bridge error:" (:message message))
         {:db db})
       
       ;; Unknown message type
       (do
         (println "Unknown bridge message type:" msg-type)
         {:db db})))))

;; -- Execute Extended Command (M-x) Implementation --

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

;; Kill/yank event handlers moved to lexicon.events.edit
;; - :yank
;; - :yank-pop

;; Undo event handlers moved to lexicon.events.edit
;; - :undo
;; - :undo-complete

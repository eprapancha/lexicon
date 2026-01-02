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
            [lexicon.events.wasm]))

;; -- Helper Functions --

;; Word boundary detection
(defn word-char?
  "Check if a character is a word character (alphanumeric or underscore)"
  [ch]
  (and ch (re-matches #"[a-zA-Z0-9_]" (str ch))))

(defn find-forward-word-boundary
  "Find the position of the next word boundary moving forward"
  [text pos]
  (let [len (count text)]
    (if (>= pos len)
      len
      (let [ch (get text pos)]
        ;; If we're on a word char, skip to end of word
        (if (word-char? ch)
          (loop [p pos]
            (if (or (>= p len) (not (word-char? (get text p))))
              p
              (recur (inc p))))
          ;; If we're on non-word char, skip to start of next word
          (loop [p pos]
            (if (or (>= p len) (word-char? (get text p)))
              p
              (recur (inc p)))))))))

(defn find-backward-word-boundary
  "Find the position of the previous word boundary moving backward"
  [text pos]
  (if (<= pos 0)
    0
    (let [ch (get text (dec pos))]
      ;; If we're after a word char, skip to beginning of word
      (if (word-char? ch)
        (loop [p (dec pos)]
          (if (or (< p 0) (not (word-char? (get text p))))
            (inc p)
            (recur (dec p))))
        ;; If we're after non-word char, skip backward to end of previous word
        (loop [p (dec pos)]
          (if (or (< p 0) (word-char? (get text p)))
            (if (< p 0)
              0
              ;; Found a word char, now find its beginning
              (loop [p2 p]
                (if (or (< p2 0) (not (word-char? (get text p2))))
                  (inc p2)
                  (recur (dec p2)))))
            (recur (dec p))))))))

;; Cursor position conversion helpers
(defn linear-pos-to-line-col
  "Convert linear position to line/column coordinates"
  [text linear-pos]
  (if (empty? text)
    {:line 0 :column 0}
    (let [lines (clojure.string/split text #"\n" -1)  ; -1 keeps trailing empty strings
          lines-with-lengths (map count lines)]
      (loop [pos 0
             line 0
             remaining-lengths lines-with-lengths]
        (if (empty? remaining-lengths)
          {:line (max 0 (dec line)) :column 0}
          (let [line-len (first remaining-lengths)
                line-end-pos (+ pos line-len)]
            (if (<= linear-pos line-end-pos)
              {:line line :column (- linear-pos pos)}
              (recur (+ line-end-pos 1) ; +1 for newline
                     (inc line)
                     (rest remaining-lengths)))))))))

(defn line-col-to-linear-pos
  "Convert line/column coordinates to linear position"
  [text line column]
  (let [lines (clojure.string/split text #"\n" -1)]  ; -1 keeps trailing empty strings
    (if (>= line (count lines))
      (count text)
      (let [lines-before (take line lines)
            chars-before-line (reduce + 0 (map count lines-before))
            newlines-before line ; One newline per line before current
            line-content (nth lines line "")
            safe-column (min column (count line-content))]
        (+ chars-before-line newlines-before safe-column)))))

;; Language detection helper
(defn detect-language-from-filename
  "Detect language from file name/extension"
  [filename]
  (cond
    (re-matches #".*\.(js|jsx|mjs|cjs)$" filename) :javascript
    (re-matches #".*\.(ts|tsx)$" filename) :typescript
    (re-matches #".*\.(clj|cljs|cljc|edn)$" filename) :clojure
    (re-matches #".*\.(py)$" filename) :python
    (re-matches #".*\.(rs)$" filename) :rust
    (re-matches #".*\.(md|markdown)$" filename) :markdown
    :else :text))

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

;; -- Character Deletion Commands --

(rf/reg-event-fx
 :delete-backward-char
 (fn [{:keys [db]} [_]]
   "Delete character before cursor (backspace) - queue operation"
   {:fx [[:dispatch [:editor/queue-transaction {:op :delete-backward}]]]}))

(rf/reg-event-fx
 :delete-forward-char
 (fn [{:keys [db]} [_]]
   "Delete character after cursor (delete) - queue operation"
   {:fx [[:dispatch [:editor/queue-transaction {:op :delete-forward}]]]}))

;; -- Cursor Movement Commands --

(rf/reg-event-fx
 :forward-char
 (fn [{:keys [db]} [_]]
   "Move cursor forward one character (C-f or Right arrow)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [max-pos (.length wasm-instance)
             new-pos (min max-pos (inc current-pos))]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

(rf/reg-event-fx
 :backward-char
 (fn [{:keys [db]} [_]]
   "Move cursor backward one character (C-b or Left arrow)"
   (let [current-pos (get-in db [:ui :cursor-position] 0)
         new-pos (max 0 (dec current-pos))]
     {:fx [[:dispatch [:update-cursor-position new-pos]]]})))

(rf/reg-event-fx
 :next-line
 (fn [{:keys [db]} [_]]
   "Move cursor to next line (C-n or Down arrow)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)
             lines (clojure.string/split text #"\n" -1)
             next-line-num (inc line)]
         (if (< next-line-num (count lines))
           (let [new-pos (line-col-to-linear-pos text next-line-num column)]
             {:fx [[:dispatch [:update-cursor-position new-pos]]]})
           {:db db}))
       {:db db}))))

(rf/reg-event-fx
 :previous-line
 (fn [{:keys [db]} [_]]
   "Move cursor to previous line (C-p or Up arrow)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)]
         (if (> line 0)
           (let [prev-line-num (dec line)
                 new-pos (line-col-to-linear-pos text prev-line-num column)]
             {:fx [[:dispatch [:update-cursor-position new-pos]]]})
           {:db db}))
       {:db db}))))

(rf/reg-event-fx
 :click-to-position
 (fn [{:keys [db]} [_ line column]]
   "Move cursor to clicked position"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             new-pos (line-col-to-linear-pos text line column)]
         (println "🖱️ Click position - line:" line "col:" column "→ linear pos:" new-pos)
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

(rf/reg-event-fx
 :beginning-of-line
 (fn [{:keys [db]} [_]]
   "Move cursor to beginning of current line (C-a)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line]} (linear-pos-to-line-col text current-pos)
             new-pos (line-col-to-linear-pos text line 0)]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

(rf/reg-event-fx
 :end-of-line
 (fn [{:keys [db]} [_]]
   "Move cursor to end of current line (C-e)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line]} (linear-pos-to-line-col text current-pos)
             lines (clojure.string/split text #"\n" -1)
             line-length (count (nth lines line ""))
             new-pos (line-col-to-linear-pos text line line-length)]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

(rf/reg-event-fx
 :beginning-of-buffer
 (fn [{:keys [db]} [_]]
   "Move cursor to beginning of buffer (M-<)"
   {:fx [[:dispatch [:update-cursor-position 0]]]}))

(rf/reg-event-fx
 :end-of-buffer
 (fn [{:keys [db]} [_]]
   "Move cursor to end of buffer (M->)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])]
     (if wasm-instance
       (let [buffer-length (.length wasm-instance)]
         {:fx [[:dispatch [:update-cursor-position buffer-length]]]})
       {:db db}))))

(rf/reg-event-fx
 :forward-word
 (fn [{:keys [db]} [_]]
   "Move cursor forward one word (M-f)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             new-pos (find-forward-word-boundary text current-pos)]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

(rf/reg-event-fx
 :backward-word
 (fn [{:keys [db]} [_]]
   "Move cursor backward one word (M-b)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             new-pos (find-backward-word-boundary text current-pos)]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

;; -- Editing Commands --

(rf/reg-event-fx
 :kill-line
 (fn [{:keys [db]} [_]]
   "Kill from cursor to end of line (C-k)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             buffer-length (.length wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)
             lines (clojure.string/split text #"\n" -1)
             current-line (nth lines line "")
             line-length (count current-line)
             end-of-line? (>= column line-length)
             at-end-of-buffer? (>= current-pos buffer-length)
             kill-start current-pos
             kill-end (if (and end-of-line? (not at-end-of-buffer?))
                       (inc current-pos)  ; Kill the newline (only if not at end of buffer)
                       (line-col-to-linear-pos text line line-length))
             length (- kill-end kill-start)]
         (if (> length 0)
           (let [killed-text (.getRange wasm-instance kill-start kill-end)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))]
             {:db (assoc db :kill-ring updated-kill-ring)
              :fx [[:dispatch [:editor/queue-transaction
                              {:op :delete-range :start kill-start :length length}]]]})
           {:db db}))
       {:db db}))))

(rf/reg-event-fx
 :open-line
 (fn [{:keys [db]} [_]]
   "Insert a newline after cursor without moving cursor (C-o)"
   (let [current-pos (get-in db [:ui :cursor-position] 0)]
     {:fx [[:dispatch [:editor/queue-transaction
                      {:op :insert :text "\n"}]]
           ;; After insertion, move cursor back to original position
           [:dispatch-later [{:ms 50 :dispatch [:update-cursor-position current-pos]}]]]})))

;; -- Mark and Region Commands --

(rf/reg-event-fx
 :set-mark-command
 (fn [{:keys [db]} [_]]
   "Set mark at current cursor position (C-SPC)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         current-pos (get-in db [:ui :cursor-position] 0)
         new-tree (db/update-window-in-tree window-tree active-window-id
                                            #(assoc % :mark-position current-pos))]
     (println "✓ Mark set at position" current-pos)
     {:db (assoc db :window-tree new-tree)})))

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

(rf/reg-event-fx
 :copy-region-as-kill
 (fn [{:keys [db]} [_]]
   "Copy region to kill ring without deleting (M-w)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         mark-position (:mark-position active-window)
         cursor-pos (get-in db [:ui :cursor-position] 0)]

     (if (and wasm-instance mark-position)
       (let [start (min cursor-pos mark-position)
             end (max cursor-pos mark-position)
             length (- end start)]
         (if (> length 0)
           (let [copied-text (.getRange wasm-instance start end)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons copied-text kill-ring))
                 new-tree (db/update-window-in-tree window-tree active-window-id
                                                    #(assoc % :mark-position nil))]
             (println "✓ Copied" length "characters to kill ring")
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc :window-tree new-tree))})
           {:db db}))
       (do
         (println "⚠ No region selected (set mark with C-SPC first)")
         {:db db})))))

;; Initialize cursor position when buffer is created
(rf/reg-event-fx
 :initialize-buffer-cursor
 (fn [{:keys [db]} [_ buffer-id]]
   "Initialize cursor position for a buffer"
   (let [^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])]
     (if wasm-instance
       {:fx [[:dispatch [:update-cursor-position 0]]]}
       {:db db}))))

;; -- Cursor Position Management --

(rf/reg-event-fx
 :update-cursor-position
 (fn [{:keys [db]} [_ new-linear-pos]]
   "Update cursor position in both linear and line/column formats"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             content-length (count text)
             ;; Clamp cursor position to valid range [0, content-length]
             clamped-pos (max 0 (min new-linear-pos content-length))
             line-col (linear-pos-to-line-col text clamped-pos)
             new-tree (db/update-window-in-tree window-tree active-window-id
                                                #(assoc % :cursor-position line-col))]
         {:db (-> db
                  (assoc :window-tree new-tree)
                  (assoc-in [:buffers active-buffer-id :cursor-position] line-col)  ; Save to buffer too
                  (assoc-in [:ui :cursor-position] clamped-pos))})  ; Keep old for compatibility
       {:db db}))))


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

;; -- Buffer Management Events --

(rf/reg-event-db
 :create-buffer
 (fn [db [_ name wasm-instance]]
   "Create a new buffer with the given name and WASM instance"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         new-buffer (db/create-buffer buffer-id name wasm-instance)]
     (assoc-in db [:buffers buffer-id] new-buffer))))

(rf/reg-event-fx
 :switch-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Switch to the specified buffer by updating the active window"
   (if (get-in db [:buffers buffer-id])
     (let [active-window-id (:active-window-id db)
           window-tree (:window-tree db)
           active-window (db/find-window-in-tree window-tree active-window-id)
           current-buffer-id (:buffer-id active-window)
           current-cursor (:cursor-position active-window)
           ;; Get target buffer's saved cursor position (or default to 0:0)
           target-cursor (get-in db [:buffers buffer-id :cursor-position] {:line 0 :column 0})
           ;; Save current cursor to current buffer
           db-with-saved-cursor (if current-buffer-id
                                  (assoc-in db [:buffers current-buffer-id :cursor-position] current-cursor)
                                  db)
           ;; Update window with new buffer and load its cursor
           new-tree (db/update-window-in-tree window-tree active-window-id
                                              #(assoc %
                                                      :buffer-id buffer-id
                                                      :cursor-position target-cursor))
           ;; Convert line/col to linear position for global state
           ^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
           text (when wasm-instance (.getText wasm-instance))
           linear-pos (if text
                       (line-col-to-linear-pos text (:line target-cursor) (:column target-cursor))
                       0)]
       {:db (-> db-with-saved-cursor
                (assoc :window-tree new-tree)
                (assoc-in [:ui :cursor-position] linear-pos))
        :fx [[:focus-editor]]})
     {:db db}))) ; Ignore if buffer doesn't exist

(rf/reg-event-fx
 :switch-to-buffer
 (fn [{:keys [db]} [_]]
   "Activate minibuffer for buffer switching (C-x b) with metadata"
   (let [buffers (:buffers db)
         buffer-names (sort (map :name (vals buffers)))
         current-buffer (get buffers (get-in db [:windows (:active-window-id db) :buffer-id]))
         current-name (:name current-buffer)
         prompt (str "Switch to buffer (default " current-name "): ")
         ;; Create metadata for buffer completion
         metadata (completion-metadata/make-metadata
                   :category :buffer
                   :annotation-function :buffer
                   :affixation-function :buffer
                   :display-sort-function :recent-first)]
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt prompt
                        :completions buffer-names
                        :metadata metadata
                        :on-confirm [:switch-to-buffer-by-name]}]]]})))

(rf/reg-event-fx
 :switch-to-buffer-by-name
 (fn [{:keys [db]} [_ buffer-name]]
   "Switch to buffer by name, creating it if it doesn't exist"
   (let [buffers (:buffers db)
         current-buffer-id (get-in db [:windows (:active-window-id db) :buffer-id])
         current-buffer (get buffers current-buffer-id)
         ;; If input is empty, use current buffer name
         target-name (if (clojure.string/blank? buffer-name)
                      (:name current-buffer)
                      buffer-name)
         ;; Find buffer with matching name
         target-buffer (first (filter #(= (:name %) target-name) (vals buffers)))
         target-id (:id target-buffer)]
     (if target-id
       ;; Buffer exists - switch to it
       (do
         {:fx [[:dispatch [:switch-buffer target-id]]]})
       ;; Buffer doesn't exist - create it then switch to it
       (do
         (let [buffer-id (db/next-buffer-id buffers)
               WasmEditorCore (get-in db [:system :wasm-constructor])
               wasm-instance (WasmEditorCore. "")
               new-buffer {:id buffer-id
                          :wasm-instance wasm-instance
                          :file-handle nil
                          :name target-name
                          :is-modified? false
                          :mark-position nil
                          :cursor-position {:line 0 :column 0}
                          :selection-range nil
                          :major-mode :fundamental-mode
                          :minor-modes #{}
                          :buffer-local-vars {}
                          :ast nil
                          :language :text
                          :diagnostics []
                          :undo-stack []
                          :undo-in-progress? false
                          :editor-version 0
                          :cache {:text ""
                                  :line-count 1}}]
           {:db (assoc-in db [:buffers buffer-id] new-buffer)
            :fx [[:dispatch [:switch-buffer buffer-id]]]}))))))

(rf/reg-event-fx
 :kill-buffer
 (fn [{:keys [db]} [_]]
   "Activate minibuffer for buffer killing (C-x k)"
   (let [current-buffer (get (:buffers db) (get-in db [:windows (:active-window-id db) :buffer-id]))
         current-name (:name current-buffer)
         prompt (str "Kill buffer (default " current-name "): ")]
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt prompt
                        :on-confirm [:kill-buffer-by-name]}]]]})))

(rf/reg-event-fx
 :kill-buffer-by-name
 (fn [{:keys [db]} [_ buffer-name]]
   "Kill buffer by name, or kill current if empty"
   (let [buffers (:buffers db)
         current-buffer-id (get-in db [:windows (:active-window-id db) :buffer-id])
         current-buffer (get buffers current-buffer-id)
         ;; If input is empty, use current buffer name
         target-name (if (clojure.string/blank? buffer-name)
                      (:name current-buffer)
                      buffer-name)
         ;; Find buffer with matching name
         target-buffer (first (filter #(= (:name %) target-name) (vals buffers)))
         target-id (:id target-buffer)]
     (if target-id
       ;; Don't allow killing the last buffer
       (if (= (count buffers) 1)
         {:fx []} ; Silently ignore
         (let [;; Run kill-buffer hooks
               hook-commands (get-in db [:hooks :kill-buffer-hook] [])
               ;; If killing active buffer, switch to another one first
               need-switch? (= target-id current-buffer-id)
               ;; Find another buffer to switch to
               other-buffer-id (first (filter #(not= % target-id) (keys buffers)))
               updated-db (-> db
                              ;; Remove the buffer
                              (update :buffers dissoc target-id))]
           {:db updated-db
            :fx (concat
                  ;; Run hook commands first
                  (map (fn [cmd] [:dispatch [:execute-command cmd]]) hook-commands)
                  ;; Switch to another buffer if needed
                  (when (and need-switch? other-buffer-id)
                    [[:dispatch [:switch-buffer other-buffer-id]]]))}))
       ;; Buffer not found
       {:fx []}))))

(rf/reg-event-fx
 :list-buffers
 (fn [{:keys [db]} [_]]
   "Display list of all buffers (C-x C-b)"
   (let [buffers (:buffers db)
         ;; Check if *Buffer List* buffer already exists
         buffer-list-buffer (first (filter #(= (:name %) "*Buffer List*") (vals buffers)))]
     (if buffer-list-buffer
       ;; Just switch to existing buffer list
       {:fx [[:dispatch [:switch-buffer (:id buffer-list-buffer)]]]}
       ;; Create new buffer list buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmEditorCore (get-in db [:system :wasm-constructor])
             ;; Generate buffer list content
             buffer-lines (map (fn [buf]
                                (str (if (:is-modified? buf) " *" "  ")
                                     " "
                                     (:name buf)
                                     "  "
                                     (or (:file-handle buf) "")))
                              (sort-by :id (vals buffers)))
             header "MR Buffer           File\n-- ------           ----\n"
             content (str header (clojure.string/join "\n" buffer-lines))
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (WasmEditorCore. content)]

         (let [new-buffer {:id buffer-id
                          :wasm-instance wasm-instance
                          :file-handle nil
                          :name "*Buffer List*"
                          :is-modified? false
                          :mark-position nil
                          :cursor-position {:line 0 :column 0}
                          :selection-range nil
                          :major-mode :buffer-menu-mode
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

(rf/reg-event-fx
 :buffer-menu/select-buffer
 (fn [{:keys [db]} [_]]
   "Select buffer at current line in buffer-menu-mode (RET key)"
   (let [active-buffer-id (get-in db [:windows (:active-window-id db) :buffer-id])
         active-buffer (get-in db [:buffers active-buffer-id])
         cursor-line (get-in active-buffer [:cursor-position :line] 0)]
     ;; Skip header lines (first 2 lines)
     (if (< cursor-line 2)
       {:db db}
       (let [buffer-content (get-in active-buffer [:cache :text] "")
             lines (clojure.string/split buffer-content #"\n" -1)
             selected-line (nth lines cursor-line nil)
             ;; Extract buffer name from line (format: " * BufferName  FilePath")
             ;; Buffer name starts at column 4 and ends before two consecutive spaces
             buffer-name (when selected-line
                          (let [trimmed (subs selected-line 4) ; Skip "MR " prefix
                                name-part (first (clojure.string/split trimmed #"\s{2,}"))]
                            (clojure.string/trim name-part)))
             ;; Find the buffer with this name
             target-buffer (when buffer-name
                            (first (filter #(= (:name %) buffer-name) (vals (:buffers db)))))
             target-id (:id target-buffer)]
         (if target-id
           {:fx [[:dispatch [:switch-buffer target-id]]
                 [:dispatch [:kill-buffer-by-id (:id active-buffer)]]]}
           {:db db}))))))

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

(rf/reg-event-db
 :set-cursor-position
 (fn [db [_ position]]
   "Update cursor position"
   (assoc-in db [:ui :cursor-position] position)))

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

(rf/reg-event-fx
 :save-buffer
 (fn [{:keys [db]} [_]]
   "Save the active buffer to disk (C-x C-s)"
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

(rf/reg-event-fx
 :write-file
 (fn [{:keys [db]} [_]]
   "Save buffer to a new file (C-x C-w - Save As)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]

     (if wasm-instance
       (let [content (.getText ^js wasm-instance)]
         ;; Always prompt for save location (save as)
         {:fx [[:save-file-picker {:content content
                                   :buffer-id active-buffer-id}]]})
       {:db db}))))

(rf/reg-fx
 :save-file-picker
 (fn [{:keys [content buffer-id]}]
   (-> (js/window.showSaveFilePicker)
       (.then (fn [file-handle]
                (-> (.createWritable file-handle)
                    (.then (fn [writable]
                             (-> (.write writable content)
                                 (.then (fn []
                                          (.close writable)
                                          (rf/dispatch [:buffer-saved 
                                                       {:buffer-id buffer-id
                                                        :file-handle file-handle}])))
                                 (.catch (fn [error]
                                           (println "Write failed:" error))))))
                    (.catch (fn [error]
                              (println "Failed to create writable stream:" error))))))
       (.catch (fn [error]
                 (println "Save cancelled or failed:" error))))))

(rf/reg-fx
 :save-to-file-handle
 (fn [{:keys [file-handle content buffer-id]}]
   (-> (.createWritable file-handle)
       (.then (fn [writable]
                (-> (.write writable content)
                    (.then (fn []
                             (.close writable)
                             (rf/dispatch [:buffer-saved 
                                          {:buffer-id buffer-id
                                           :file-handle file-handle}])))
                    (.catch (fn [error]
                              (println "Write failed:" error))))))
       (.catch (fn [error]
                 (println "Failed to create writable stream:" error))))))

(rf/reg-event-fx
 :buffer-saved
 (fn [{:keys [db]} [_ {:keys [buffer-id file-handle]}]]
   "Mark buffer as saved and update file handle"
   (let [file-name (.-name file-handle)]
     {:db (-> db
              (assoc-in [:buffers buffer-id :is-modified?] false)
              (assoc-in [:buffers buffer-id :file-handle] file-handle)
              (assoc-in [:buffers buffer-id :name] file-name))
      :fx [[:dispatch [:lsp/on-buffer-saved buffer-id]]
           [:dispatch [:echo/message (str "Wrote " file-name)]]]})))

(rf/reg-event-fx
 :find-file
 (fn [{:keys [db]} [_]]
   "Open a file from disk"
   {:fx [[:open-file-picker]]}))

(rf/reg-fx
 :open-file-picker
 (fn [_]
   "Handle file picker interaction and dispatch appropriate events"
   (-> (js/window.showOpenFilePicker)
       (.then (fn [file-handles]
                (let [file-handle (first file-handles)]
                  (-> (.getFile file-handle)
                      (.then (fn [file]
                               (-> (.text file)
                                   (.then (fn [content]
                                            (rf/dispatch [:file-read-success 
                                                         {:file-handle file-handle
                                                          :content content
                                                          :name (.-name file)}])))
                                   (.catch (fn [error]
                                             (rf/dispatch [:file-read-failure 
                                                          {:error error
                                                           :message "Failed to read file content"}]))))))
                      (.catch (fn [error]
                                (rf/dispatch [:file-read-failure 
                                             {:error error
                                              :message "Failed to access file"}])))))))
       (.catch (fn [error]
                 ;; Don't dispatch error for user cancellation
                 (when (not= (.-name error) "AbortError")
                   (rf/dispatch [:file-read-failure 
                                {:error error
                                 :message "File picker failed"}])))))))

(rf/reg-event-fx
 :file-read-success
 (fn [{:keys [db]} [_ {:keys [file-handle content name]}]]
   "Handle successful file read - create new buffer and switch to it"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         WasmEditorCore (get-in db [:system :wasm-constructor])
         wasm-instance (WasmEditorCore. content)
         detected-language (detect-language-from-filename name)
         lines (clojure.string/split content #"\n" -1)
         line-count (count lines)
           new-buffer {:id buffer-id
                       :wasm-instance wasm-instance
                       :file-handle file-handle
                       :name name
                       :is-modified? false
                       :mark-position nil
                       :cursor-position {:line 0 :column 0}
                       :selection-range nil
                       :major-mode :fundamental-mode
                       :minor-modes #{}
                       :buffer-local-vars {}
                       :ast nil
                       :language detected-language
                       :diagnostics []
                       :undo-stack []
                       :undo-in-progress? false
                       :editor-version 0
                       :cache {:text content
                               :line-count line-count}}]
       {:db (assoc-in db [:buffers buffer-id] new-buffer)
        :fx [[:dispatch [:switch-buffer buffer-id]]
             [:dispatch [:parser/request-parse buffer-id]]
             [:dispatch [:lsp/on-buffer-opened buffer-id]]]})))

;; Debug helper to check parser worker state
(rf/reg-event-fx
 :debug/check-parser-state
 (fn [{:keys [db]} [_]]
   (let [worker (get-in db [:system :parser-worker])
         worker-ready? (get-in db [:system :parser-worker-ready?])]
     (println "🔍 Debug - Parser worker:" (if worker "exists" "missing") 
              "Ready:" worker-ready?)
     {:db db})))

(rf/reg-event-db
 :file-read-failure
 (fn [db [_ {:keys [error message]}]]
   "Handle file read failure"
   (println "File read failed:" message error)
   ;; Could add user notification here in the future
   db))

;; -- Buffer Lifecycle Events --

(rf/reg-event-fx
 :close-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Close a buffer and free its WASM memory, with defensive logic"
   (let [buffers (:buffers db)
         buffer-to-close (get buffers buffer-id)
         active-window-id (:active-window-id db)
         active-window (get (:windows db) active-window-id)
         currently-active-buffer-id (:buffer-id active-window)]
     
     (if buffer-to-close
       (let [wasm-instance (:wasm-instance buffer-to-close)
             remaining-buffers (dissoc buffers buffer-id)
             remaining-buffer-ids (keys remaining-buffers)]
         
         ;; Free the WASM instance memory
         (when wasm-instance
           (.free ^js wasm-instance))
         
         (if (empty? remaining-buffer-ids)
           ;; No buffers left - create a new default *scratch* buffer
           (let [new-buffer-id (inc (apply max 0 (keys buffers)))
                 WasmEditorCore (get-in db [:system :wasm-constructor])
                 new-wasm-instance (WasmEditorCore. "")]
             {:db (-> db
                      (assoc :buffers {new-buffer-id {:id new-buffer-id
                                                      :wasm-instance new-wasm-instance
                                                      :file-handle nil
                                                      :name "*scratch*"
                                                      :is-modified? false
                                                      :mark-position nil}})
                      (assoc-in [:windows active-window-id :buffer-id] new-buffer-id))})
           
           ;; Other buffers exist - switch to another buffer if necessary
           (let [new-active-buffer-id (if (= buffer-id currently-active-buffer-id)
                                        ;; Need to switch to a different buffer
                                        (first remaining-buffer-ids)
                                        ;; Keep current active buffer
                                        currently-active-buffer-id)]
             {:db (-> db
                      (assoc :buffers remaining-buffers)
                      (assoc-in [:windows active-window-id :buffer-id] new-active-buffer-id))})))
       
       ;; Buffer not found - no action needed
       {:db db}))))

;; -- WASM Loading Error Events --

(rf/reg-event-fx
 :reconcile-dom-if-needed
 (fn [{:keys [db]} [_ dom-content]]
   "Check if DOM content differs from WASM content and reconcile if needed"
   (let [reconciliation-active? (get-in db [:system :reconciliation-active?])
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     ;; Only process if reconciliation is not currently active
     (when (and (not reconciliation-active?) wasm-instance)
       (let [expected-content (.getText ^js wasm-instance)]
         (when (not= dom-content expected-content)
           (println "🔄 Reconciling DOM state")
           {:fx [[:dispatch [:reconcile-dom-state dom-content expected-content]]]})))
     {:db db})))

;; -- Viewport Events for Virtualized Rendering --

(rf/reg-event-db
 :update-viewport
 (fn [db [_ start-line end-line]]
   "Update the viewport for the active window"
   (let [active-window-id (:active-window-id db)]
     (-> db
         (assoc-in [:windows active-window-id :viewport :start-line] start-line)
         (assoc-in [:windows active-window-id :viewport :end-line] end-line)))))

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

;; -- Region Selection and Kill Ring Events --

(rf/reg-event-db
 :set-mark
 (fn [db [_]]
   "Set the mark at the current cursor position"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         cursor-pos (get-in db [:ui :cursor-position])
         new-tree (db/update-window-in-tree window-tree active-window-id
                                            #(assoc % :mark-position cursor-pos))]
     (assoc db :window-tree new-tree))))

(rf/reg-event-fx
 :kill-region
 (fn [{:keys [db]} [_]]
   "Kill (cut) the region between point and mark"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         mark-position (:mark-position active-window)
         cursor-pos (get-in db [:ui :cursor-position])]

     (if (and wasm-instance mark-position)
       (let [start (min cursor-pos mark-position)
             end (max cursor-pos mark-position)
             length (- end start)]
         (if (> length 0)
           (let [killed-text (.getRange ^js wasm-instance start end)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))
                 new-tree (db/update-window-in-tree window-tree active-window-id
                                                    #(assoc % :mark-position nil))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc :window-tree new-tree))
              :fx [[:dispatch [:editor/queue-transaction
                              {:op :delete-range :start start :length length}]]]})
           {:db db}))
       (do
         (println "Mark not set")
         {:db db})))))

(rf/reg-event-fx
 :yank
 (fn [{:keys [db]} [_]]
   "Yank (paste) the most recent kill"
   (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer    (get (:buffers db) active-buffer-id)
         wasm-instance    (:wasm-instance active-buffer)
         kill-ring        (:kill-ring db)
         cursor-pos       (get-in db [:ui :cursor-position])]

     (if (and wasm-instance (seq kill-ring))
       (let [text-to-yank (first kill-ring)
             yank-length (count text-to-yank)]
         (println "✓ Yanking" yank-length "chars at position" cursor-pos)
         {:db (-> db
                  (assoc :last-yank {:position cursor-pos
                                     :length yank-length
                                     :kill-ring-index 0})
                  (assoc :last-command :yank))
          :fx [[:dispatch [:editor/queue-transaction
                           {:op :insert :text text-to-yank}]]]})
       {:db db}))))

(rf/reg-event-fx
 :yank-pop
 (fn [{:keys [db]} [_]]
   "Replace last yank with next item in kill ring (M-y)"
   (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer    (get (:buffers db) active-buffer-id)
         wasm-instance    (:wasm-instance active-buffer)
         kill-ring        (:kill-ring db)
         last-yank        (:last-yank db)
         last-command     (:last-command db)]

     ;; Only allow yank-pop immediately after yank or yank-pop
     (if (and wasm-instance
              last-yank
              (seq kill-ring)
              (or (= last-command :yank) (= last-command :yank-pop)))
       (let [{:keys [position length kill-ring-index]} last-yank
             next-index (mod (inc kill-ring-index) (count kill-ring))
             text-to-yank (nth kill-ring next-index)
             new-length (count text-to-yank)]
         (println "✓ Yank-pop: replacing" length "chars with" new-length "chars at position" position)
         {:db (-> db
                  (assoc :last-yank {:position position
                                     :length new-length
                                     :kill-ring-index next-index})
                  (assoc :last-command :yank-pop))
          :fx [[:dispatch [:editor/queue-transaction
                          {:op :replace :start position :length length :text text-to-yank}]]]})
       (do
         (println "⚠ Cannot yank-pop: only works immediately after yank")
         {:db db})))))

;; -- Undo Command --

(rf/reg-event-fx
 :undo
 (fn [{:keys [db]} [_]]
   "Undo the last operation (C-/ or C-_)"
   (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer    (get (:buffers db) active-buffer-id)
         wasm-instance    (:wasm-instance active-buffer)
         undo-stack       (:undo-stack active-buffer)]

     (if (and wasm-instance (seq undo-stack))
       (let [undo-entry (peek undo-stack)
             updated-stack (pop undo-stack)]
         (println "⏪ Undo: applying" undo-entry)
         {:db (-> db
                  (assoc-in [:buffers active-buffer-id :undo-stack] updated-stack)
                  (assoc-in [:buffers active-buffer-id :undo-in-progress?] true))
          :fx [[:dispatch [:editor/queue-transaction undo-entry]]
               [:dispatch-later [{:ms 10 :dispatch [:undo-complete active-buffer-id]}]]]})
       (do
         (println "⚠ Nothing to undo")
         {:db db})))))

(rf/reg-event-db
 :undo-complete
 (fn [db [_ buffer-id]]
   "Reset undo-in-progress flag after undo operation completes"
   (assoc-in db [:buffers buffer-id :undo-in-progress?] false)))

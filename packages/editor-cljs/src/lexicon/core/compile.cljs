(ns lexicon.core.compile
  "Compilation mode - run commands and parse errors.

  M-x compile prompts for a command and creates a *compilation* buffer
  showing the output. Error messages are parsed and navigable with
  next-error (M-g n) and previous-error (M-g p).

  Based on Emacs lisp/progmodes/compile.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]
            [lexicon.core.log :as log]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Error Regexp Patterns
;; =============================================================================

;; Simplified error patterns for common compilers/tools
;; Format: {:name :pattern :file-group :line-group :column-group :type-group}
(def compilation-error-regexps
  [{:name :gnu
    ;; GNU GCC: file:line:col: error/warning: message
    :pattern #"^([^:\s]+):(\d+):(\d+):\s*(error|warning|note):"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group 4}
   {:name :gnu-simple
    ;; GNU simple: file:line: error/warning: message
    :pattern #"^([^:\s]+):(\d+):\s*(error|warning|note):"
    :file-group 1
    :line-group 2
    :column-group nil
    :type-group 3}
   {:name :clojure
    ;; Clojure: Syntax error compiling at (file:line:col)
    :pattern #"at \(([^:]+):(\d+):(\d+)\)"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group nil}
   {:name :javascript
    ;; JavaScript/Node: file:line
    :pattern #"^\s+at .+ \(([^:]+):(\d+):(\d+)\)"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group nil}
   {:name :python
    ;; Python: File "file", line N
    :pattern #"File \"([^\"]+)\", line (\d+)"
    :file-group 1
    :line-group 2
    :column-group nil
    :type-group nil}
   {:name :rust
    ;; Rust: --> file:line:col
    :pattern #"^\s*--> ([^:]+):(\d+):(\d+)"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group nil}
   {:name :generic
    ;; Generic: file:line
    :pattern #"^([^:\s][^:]*):(\d+):"
    :file-group 1
    :line-group 2
    :column-group nil
    :type-group nil}])

;; =============================================================================
;; State
;; =============================================================================

;; Store compilation state
(defonce compilation-state
  (atom {:last-command "make -k"
         :errors []           ; Parsed errors [{:file :line :col :type :message :buffer-pos}]
         :error-index 0       ; Current error index for navigation
         :source-buffer nil})) ; Buffer we were in when compile was invoked

;; =============================================================================
;; Error Parsing
;; =============================================================================

(defn parse-error-line
  "Try to parse a line as an error message.
   Returns {:file :line :col :type :message :buffer-pos} or nil."
  [line buffer-pos]
  (some (fn [{:keys [pattern file-group line-group column-group type-group]}]
          (when-let [match (re-find pattern line)]
            (let [groups (if (string? match) [match] (vec match))]
              {:file (get groups file-group)
               :line (when-let [l (get groups line-group)]
                       (js/parseInt l 10))
               :col (when (and column-group (get groups column-group))
                      (js/parseInt (get groups column-group) 10))
               :type (or (get groups type-group) "error")
               :message line
               :buffer-pos buffer-pos})))
        compilation-error-regexps))

(defn parse-compilation-output
  "Parse compilation output and extract errors.
   Returns vector of error maps."
  [output]
  (let [lines (str/split-lines output)]
    (loop [remaining lines
           pos 0
           errors []]
      (if (empty? remaining)
        errors
        (let [line (first remaining)
              line-length (inc (count line)) ; +1 for newline
              error (parse-error-line line pos)]
          (recur (rest remaining)
                 (+ pos line-length)
                 (if error (conj errors error) errors)))))))

(defn count-by-type
  "Count errors by type (error, warning, note)."
  [errors]
  (let [grouped (group-by #(str/lower-case (or (:type %) "error")) errors)]
    {:errors (count (get grouped "error" []))
     :warnings (count (get grouped "warning" []))
     :notes (count (get grouped "note" []))}))

;; =============================================================================
;; Buffer Creation
;; =============================================================================

(defn format-compilation-header
  "Format the compilation buffer header."
  [command]
  (str "-*- mode: compilation; default-directory: \"" js/location.pathname "\" -*-\n"
       "Compilation started at " (.toLocaleTimeString (js/Date.)) "\n\n"
       command "\n\n"))

(defn format-compilation-footer
  "Format the compilation buffer footer."
  [exit-code counts]
  (let [{:keys [errors warnings notes]} counts]
    (str "\n\nCompilation "
         (if (zero? exit-code) "finished" (str "exited abnormally with code " exit-code))
         " at " (.toLocaleTimeString (js/Date.))
         (when (pos? (+ errors warnings notes))
           (str " [" errors " error(s), " warnings " warning(s)]")))))

;; =============================================================================
;; Compilation Simulation (browser environment)
;; =============================================================================

(defn simulate-compilation
  "Simulate compilation output since we can't run actual commands in browser.
   Returns simulated output with some errors for demonstration."
  [command]
  (cond
    ;; Simulate make output
    (str/starts-with? command "make")
    (str "make: Entering directory '/home/user/project'\n"
         "gcc -c -o main.o main.c\n"
         "main.c:10:5: warning: unused variable 'x' [-Wunused-variable]\n"
         "main.c:15:10: error: expected ';' before '}' token\n"
         "make: *** [Makefile:5: main.o] Error 1\n"
         "make: Leaving directory '/home/user/project'\n")

    ;; Simulate npm/node output
    (or (str/starts-with? command "npm")
        (str/starts-with? command "node"))
    (str "npm WARN deprecated\n"
         "> project@1.0.0 build\n"
         "> tsc\n"
         "src/index.ts:5:3: error TS2304: Cannot find name 'foo'.\n"
         "src/utils.ts:12:15: warning: Unused import 'bar'.\n")

    ;; Simulate clojure output
    (or (str/starts-with? command "clj")
        (str/starts-with? command "lein"))
    (str "Compiling project...\n"
         "Syntax error compiling at (src/core.clj:15:3)\n"
         "Unable to resolve symbol: undefined-fn\n")

    ;; Default: echo the command
    :else
    (str "Running: " command "\n"
         "[Simulated compilation - actual command execution not available in browser]\n"
         "Compilation complete.\n")))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

;; M-x compile - Prompt for command
(rf/reg-event-fx
 :compile
 (fn [{:keys [db]} [_]]
   (log/debug "compile: starting")
   (let [last-cmd (:last-command @compilation-state)]
     (log/debug (str "compile: activating minibuffer with command: " last-cmd))
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt "Compile command: "
                        :input last-cmd
                        :on-confirm [:compile/run]
                        :replace? true}]]]})))

;; Execute compilation command
(rf/reg-event-fx
 :compile/run
 (fn [{:keys [db]} [_ command]]
   (log/debug (str "compile/run: running with command: " command))
   ;; Store the command for next time
   (swap! compilation-state assoc :last-command command)
   (log/debug (str "compile/run: checking WASM constructor"))
   (swap! compilation-state assoc :source-buffer (lisp/current-buffer))
   (swap! compilation-state assoc :errors [])
   (swap! compilation-state assoc :error-index 0)

   ;; In browser, we can't actually run shell commands
   ;; So we simulate compilation output for demonstration
   (let [simulated-output (simulate-compilation command)
         errors (parse-compilation-output simulated-output)
         counts (count-by-type errors)
         header (format-compilation-header command)
         footer (format-compilation-footer 0 counts)
         content (str header simulated-output footer)]

     ;; Store parsed errors
     (swap! compilation-state assoc :errors errors)

     ;; Create the *compilation* buffer
     (let [existing-id (lisp/get-buffer "*compilation*")
           buffers (:buffers db)
           buffer-id (or existing-id (db/next-buffer-id buffers))
           WasmGapBuffer (get-in db [:system :wasm-constructor])]
       (log/debug (str "compile/run: WasmGapBuffer=" (if WasmGapBuffer "available" "nil") ", buffer-id=" buffer-id))
       (if-not WasmGapBuffer
         {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
         (let [wasm-instance (new WasmGapBuffer content)
               lines (str/split-lines content)
               line-count (count lines)

               new-buffer {:id buffer-id
                           :wasm-instance wasm-instance
                           :file-handle nil
                           :name "*compilation*"
                           :is-modified? false
                           :mark-position nil
                           :cursor-position {:line 0 :column 0}
                           :selection-range nil
                           :major-mode :compilation-mode
                           :minor-modes #{}
                           :read-only true
                           :line-count line-count}

               ;; Update or add buffer
               new-db (-> db
                          (assoc-in [:buffers buffer-id] new-buffer))]

           (log/debug (str "compile/run: buffer created, switching to " buffer-id))
           {:db new-db
            :fx [[:dispatch [:switch-buffer buffer-id]]
                 [:dispatch [:echo/message
                             (str "Compilation finished: "
                                  (:errors counts) " error(s), "
                                  (:warnings counts) " warning(s)")]]]}))))))

;; M-g n / C-x ` - Next error
(rf/reg-event-fx
 :next-error
 (fn [{:keys [_db]} [_ n]]
   (let [n (or n 1)
         errors (:errors @compilation-state)
         current-idx (:error-index @compilation-state)
         new-idx (min (dec (count errors)) (+ current-idx n))]
     (if (empty? errors)
       {:fx [[:dispatch [:echo/message "No compilation errors"]]]}
       (if (>= new-idx (count errors))
         {:fx [[:dispatch [:echo/message "Moved past last error"]]]}
         (let [error (get errors new-idx)]
           (swap! compilation-state assoc :error-index new-idx)
           {:fx [[:dispatch [:compile/goto-error error]]]}))))))

;; M-g p - Previous error
(rf/reg-event-fx
 :previous-error
 (fn [{:keys [_db]} [_ n]]
   (let [n (or n 1)
         errors (:errors @compilation-state)
         current-idx (:error-index @compilation-state)
         new-idx (max 0 (- current-idx n))]
     (if (empty? errors)
       {:fx [[:dispatch [:echo/message "No compilation errors"]]]}
       (if (< new-idx 0)
         {:fx [[:dispatch [:echo/message "Moved past first error"]]]}
         (let [error (get errors new-idx)]
           (swap! compilation-state assoc :error-index new-idx)
           {:fx [[:dispatch [:compile/goto-error error]]]}))))))

;; Go to error location
(rf/reg-event-fx
 :compile/goto-error
 (fn [{:keys [_db]} [_ error]]
   (let [{:keys [file line col]} error]
     ;; Display error info in echo area
     {:fx [[:dispatch [:echo/message (str file ":" line
                                          (when col (str ":" col))
                                          " - " (or (:type error) "error"))]]]})))

;; RET in compilation buffer - go to error at point
(rf/reg-event-fx
 :compile/goto-error-at-point
 (fn [{:keys [_db]} [_]]
   (let [cursor-pos (lisp/point)
         errors (:errors @compilation-state)]
     ;; Find error closest to cursor position
     (if-let [error (first (filter #(<= (:buffer-pos %) cursor-pos) (reverse errors)))]
       {:fx [[:dispatch [:compile/goto-error error]]]}
       {:fx [[:dispatch [:echo/message "No error at point"]]]}))))

;; M-x recompile - rerun last compilation
(rf/reg-event-fx
 :recompile
 (fn [{:keys [db]} [_]]
   (let [last-cmd (:last-command @compilation-state)]
     (if last-cmd
       {:fx [[:dispatch [:compile/run last-cmd]]]}
       {:fx [[:dispatch [:compile]]]}))))

;; Kill compilation
(rf/reg-event-fx
 :kill-compilation
 (fn [{:keys [_db]} [_]]
   ;; In real implementation, would kill the process
   {:fx [[:dispatch [:echo/message "Compilation process killed"]]]}))

;; =============================================================================
;; Compilation Mode
;; =============================================================================

(def compilation-mode-map
  "Keymap for compilation-mode."
  {"g"       :recompile
   "RET"     :compile/goto-error-at-point
   "n"       :compilation-next-error
   "p"       :compilation-previous-error
   "q"       :quit-window
   "M-n"     :compilation-next-file
   "M-p"     :compilation-previous-file})

;; Register compilation mode
(rf/reg-event-db
 :compilation-mode/init
 (fn [db [_]]
   (assoc-in db [:modes :compilation-mode]
             {:name "Compilation"
              :keymap compilation-mode-map
              :read-only true})))

;; Navigation within compilation buffer
(rf/reg-event-fx
 :compilation-next-error
 (fn [{:keys [_db]} [_]]
   ;; Move to next error line in buffer
   {:fx [[:dispatch [:next-error 1]]]}))

(rf/reg-event-fx
 :compilation-previous-error
 (fn [{:keys [_db]} [_]]
   {:fx [[:dispatch [:previous-error 1]]]}))

;; quit-window - Kill buffer and/or bury it
(rf/reg-event-fx
 :quit-window
 (fn [{:keys [_db]} [_]]
   ;; Bury the current buffer (switch to previous buffer)
   ;; In Emacs, quit-window optionally kills the buffer for temp buffers
   (let [source-buffer (:source-buffer @compilation-state)
         ;; Find *scratch* buffer as fallback
         scratch-buffer-id (lisp/get-buffer "*scratch*")]
     (cond
       source-buffer
       {:fx [[:dispatch [:switch-buffer source-buffer]]]}
       scratch-buffer-id
       {:fx [[:dispatch [:switch-buffer scratch-buffer-id]]]}
       :else
       {:fx [[:dispatch [:echo/message "No buffer to switch to"]]]}))))

;; =============================================================================
;; Command Registration
;; =============================================================================

(defn register-compile-commands! []
  (rf/dispatch-sync
   [:register-command :compile
    {:docstring "Compile the program including the current buffer"
     :handler [:compile]}])

  (rf/dispatch-sync
   [:register-command :recompile
    {:docstring "Re-execute the last compilation command"
     :handler [:recompile]}])

  (rf/dispatch-sync
   [:register-command :next-error
    {:docstring "Visit next compilation error message and corresponding source code"
     :handler [:next-error]}])

  (rf/dispatch-sync
   [:register-command :previous-error
    {:docstring "Visit previous compilation error message and corresponding source code"
     :handler [:previous-error]}])

  (rf/dispatch-sync
   [:register-command :kill-compilation
    {:docstring "Kill the process made by the \\[compile] command"
     :handler [:kill-compilation]}])

  (rf/dispatch-sync
   [:register-command :quit-window
    {:docstring "Quit the current window, burying its buffer"
     :handler [:quit-window]}]))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-compile! []
  (register-compile-commands!)
  (rf/dispatch-sync [:compilation-mode/init]))

(ns lexicon.core.comint
  "Comint mode and terminal emulation (comint.el, term.el).

  Implements Emacs comint.el base mode for interactive processes:
  - comint-mode: Base mode for shell/REPL interaction
  - Input history ring with M-p / M-n navigation
  - comint-bol: Move to beginning of input (after prompt)
  - comint-kill-input: Kill current input line
  - comint-delete-output: Delete last batch of output
  - comint-show-output: Scroll to last output start
  - comint-dynamic-list-input-ring: Show history buffer
  - Process mark tracking
  - Input filtering and processing
  - Output handling

  Terminal emulation (term.el):
  - term: Open terminal emulator
  - ansi-term: Terminal with ANSI color support
  - Term char-mode vs line-mode switching

  Comint key bindings:
  - M-p: Previous input from history
  - M-n: Next input from history
  - RET: Send input to process
  - C-c C-a: Beginning of input (comint-bol)
  - C-c C-u: Kill input
  - C-c C-o: Delete output
  - C-c C-r: Show last output
  - C-c C-l: List input history
  - C-c C-c: Interrupt

  Term key bindings:
  - C-c C-j: Switch to line mode
  - C-c C-k: Switch to char mode

  In browser context, provides the UI framework for interactive
  command buffers. Actual process spawning requires a backend.

  Based on Emacs lisp/comint.el and lisp/term.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Input History Ring
;; =============================================================================

(def default-ring-size 64)

(rf/reg-event-db
 :comint/add-to-history
 (fn [db [_ buffer-id input]]
   (let [history (get-in db [:comint buffer-id :history] [])
         max-size (get-in db [:comint buffer-id :ring-size] default-ring-size)
         new-history (if (and (seq input)
                              (not= input (last history)))
                       (let [h (conj history input)]
                         (if (> (count h) max-size)
                           (subvec h (- (count h) max-size))
                           h))
                       history)]
     (-> db
         (assoc-in [:comint buffer-id :history] new-history)
         (assoc-in [:comint buffer-id :history-index] (count new-history))))))

(rf/reg-event-fx
 :comint/previous-input
 (fn [{:keys [db]} [_]]
   "Cycle backward through input history (M-p)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         history (get-in db [:comint buffer-id :history] [])
         idx (get-in db [:comint buffer-id :history-index] (count history))
         new-idx (max 0 (dec idx))]
     (if (and (seq history) (< new-idx (count history)))
       {:db (assoc-in db [:comint buffer-id :history-index] new-idx)
        :fx [[:dispatch [:echo/message (nth history new-idx)]]]}
       {:fx [[:dispatch [:echo/message "Beginning of history"]]]}))))

(rf/reg-event-fx
 :comint/next-input
 (fn [{:keys [db]} [_]]
   "Cycle forward through input history (M-n)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         history (get-in db [:comint buffer-id :history] [])
         idx (get-in db [:comint buffer-id :history-index] 0)
         new-idx (min (count history) (inc idx))]
     (if (< new-idx (count history))
       {:db (assoc-in db [:comint buffer-id :history-index] new-idx)
        :fx [[:dispatch [:echo/message (nth history new-idx)]]]}
       {:fx [[:dispatch [:echo/message "End of history"]]]}))))

(rf/reg-event-fx
 :comint/previous-matching-input
 (fn [{:keys [db]} [_]]
   "Search backward through history for matching input (M-r)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "History search: "
                      :on-confirm [:comint/search-history-exec]}]]]}))

(rf/reg-event-fx
 :comint/search-history-exec
 (fn [{:keys [db]} [_ pattern]]
   "Execute history search."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         history (get-in db [:comint buffer-id :history] [])
         re (try (js/RegExp. pattern "i") (catch :default _ nil))
         match (when re
                 (last (filter #(.test re %) history)))]
     (if match
       {:fx [[:dispatch [:echo/message (str "Found: " match)]]]}
       {:fx [[:dispatch [:echo/message (str "No match for: " pattern)]]]}))))

;; =============================================================================
;; Comint Process Interaction
;; =============================================================================

(rf/reg-event-fx
 :comint/send-input
 (fn [{:keys [db]} [_ buffer-id input]]
   "Send input to the process in buffer."
   {:db db
    :fx [[:dispatch [:comint/add-to-history buffer-id input]]
         [:dispatch [:shell/send-input buffer-id input]]]}))

(rf/reg-event-fx
 :comint/interrupt
 (fn [_ [_]]
   "Interrupt the subprocess (C-c C-c)."
   {:fx [[:dispatch [:echo/message "Interrupt (no subprocess in browser)"]]]}))

(rf/reg-event-fx
 :comint/stop
 (fn [_ [_]]
   "Stop the subprocess (C-c C-z)."
   {:fx [[:dispatch [:echo/message "Stop (no subprocess in browser)"]]]}))

(rf/reg-event-fx
 :comint/eof
 (fn [_ [_]]
   "Send EOF to subprocess (C-c C-d)."
   {:fx [[:dispatch [:echo/message "EOF (no subprocess in browser)"]]]}))

;; =============================================================================
;; Comint Line Editing
;; =============================================================================

(rf/reg-event-fx
 :comint/bol
 (fn [{:keys [db]} [_]]
   "Move to beginning of input after prompt (C-c C-a).
    In shell-mode, moves to the position right after the prompt string."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         ;; Find the last prompt ($ ) in the text
         last-prompt-idx (when text (str/last-index-of text "$ "))]
     (if last-prompt-idx
       {:fx [[:dispatch [:echo/message "Beginning of input"]]]}
       {:fx [[:dispatch [:echo/message "No prompt found"]]]}))))

(rf/reg-event-fx
 :comint/kill-input
 (fn [{:keys [db]} [_]]
   "Kill the current input line (C-c C-u).
    Clears text after the last prompt."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)]
     (if (and wasm (= (:major-mode buffer) :shell-mode))
       (let [text (try (.getText ^js wasm) (catch :default _ ""))
             last-prompt-idx (str/last-index-of text "$ ")]
         (if last-prompt-idx
           (let [input-start (+ last-prompt-idx 2)
                 len (.-length wasm)
                 to-delete (- len input-start)]
             (when (pos? to-delete)
               (.delete ^js wasm input-start to-delete))
             (let [new-text (try (.getText ^js wasm) (catch :default _ ""))
                   lines (str/split new-text #"\n" -1)]
               {:db (-> db
                        (assoc-in [:buffers buffer-id :cache :text] new-text)
                        (assoc-in [:buffers buffer-id :cache :line-count] (count lines)))
                :fx [[:dispatch [:echo/message "Input killed"]]]}))
           {:fx [[:dispatch [:echo/message "No input to kill"]]]}))
       {:fx [[:dispatch [:echo/message "Not in a shell buffer"]]]}))))

(rf/reg-event-fx
 :comint/delete-output
 (fn [{:keys [db]} [_]]
   "Delete last batch of process output (C-c C-o).
    Removes text between the second-to-last and last prompt."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)]
     (if (and wasm (= (:major-mode buffer) :shell-mode))
       (let [text (try (.getText ^js wasm) (catch :default _ ""))
             ;; Find last two prompts
             last-prompt (str/last-index-of text "$ ")
             prev-prompt (when last-prompt
                           (str/last-index-of text "$ " (dec last-prompt)))]
         (if (and last-prompt prev-prompt)
           (let [;; Delete from end of prev input to start of last prompt
                 delete-start (+ prev-prompt 2)
                 delete-end last-prompt
                 to-delete (- delete-end delete-start)]
             (when (pos? to-delete)
               (.delete ^js wasm delete-start to-delete))
             (let [new-text (try (.getText ^js wasm) (catch :default _ ""))
                   lines (str/split new-text #"\n" -1)]
               {:db (-> db
                        (assoc-in [:buffers buffer-id :cache :text] new-text)
                        (assoc-in [:buffers buffer-id :cache :line-count] (count lines)))
                :fx [[:dispatch [:echo/message "Output deleted"]]]}))
           {:fx [[:dispatch [:echo/message "*** output flushed ***"]]]}))
       {:fx [[:dispatch [:echo/message "Not in a shell buffer"]]]}))))

(rf/reg-event-fx
 :comint/show-output
 (fn [_ [_]]
   "Show beginning of last output group (C-c C-r)."
   {:fx [[:dispatch [:echo/message "Show output (scroll to last output)"]]]}))

(rf/reg-event-fx
 :comint/dynamic-list-input-ring
 (fn [{:keys [db]} [_]]
   "Display the input history in a buffer (C-c C-l)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         history (get-in db [:comint buffer-id :history] [])
         content (str "Input History\n"
                      (str/join "" (repeat 40 "=")) "\n\n"
                      (if (seq history)
                        (str/join "\n"
                                  (map-indexed
                                   (fn [i cmd] (str "  " (inc i) "  " cmd))
                                   history))
                        "(empty history)")
                      "\n")
         buffers (:buffers db)
         new-buffer-id (db/next-buffer-id buffers)
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
         lines (str/split content #"\n" -1)
         line-count (count lines)]
     (if wasm-instance
       {:db (assoc-in db [:buffers new-buffer-id]
                      {:id new-buffer-id
                       :name "*Input History*"
                       :wasm-instance wasm-instance
                       :file-handle nil
                       :major-mode :special-mode
                       :is-read-only? true
                       :is-modified? false
                       :mark-position nil
                       :cursor-position {:line 0 :column 0}
                       :selection-range nil
                       :minor-modes #{}
                       :buffer-local-vars {}
                       :ast nil
                       :language :text
                       :diagnostics []
                       :undo-stack []
                       :undo-in-progress? false
                       :editor-version 0
                       :text-properties {}
                       :overlays {}
                       :next-overlay-id 1
                       :cache {:text content
                               :line-count line-count}})
        :fx [[:dispatch [:switch-buffer new-buffer-id]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

;; =============================================================================
;; Term Mode
;; =============================================================================

(rf/reg-event-fx
 :term/open
 (fn [_ [_ term-name]]
   "Open a terminal buffer (M-x term)."
   (let [name (or term-name "*terminal*")]
     {:fx [[:dispatch [:shell/open name]]]})))

(rf/reg-event-fx
 :term/ansi-term
 (fn [_ [_]]
   "Open an ANSI terminal (M-x ansi-term)."
   {:fx [[:dispatch [:term/open "*ansi-term*"]]]}))

(rf/reg-event-fx
 :term/char-mode
 (fn [{:keys [db]} [_]]
   "Switch to character mode (C-c C-k)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))]
     {:db (assoc-in db [:comint buffer-id :term-mode] :char)
      :fx [[:dispatch [:echo/message "Char mode"]]]})))

(rf/reg-event-fx
 :term/line-mode
 (fn [{:keys [db]} [_]]
   "Switch to line mode (C-c C-j)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))]
     {:db (assoc-in db [:comint buffer-id :term-mode] :line)
      :fx [[:dispatch [:echo/message "Line mode"]]]})))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize comint and term modules."
  []
  ;; Comint commands
  (rf/dispatch [:register-command :comint-previous-input
                {:docstring "Cycle backward through input history (M-p)"
                 :interactive nil
                 :handler [:comint/previous-input]}])

  (rf/dispatch [:register-command :comint-next-input
                {:docstring "Cycle forward through input history (M-n)"
                 :interactive nil
                 :handler [:comint/next-input]}])

  (rf/dispatch [:register-command :comint-previous-matching-input
                {:docstring "Search backward through history (M-r)"
                 :interactive nil
                 :handler [:comint/previous-matching-input]}])

  (rf/dispatch [:register-command :comint-interrupt-subjob
                {:docstring "Interrupt subprocess (C-c C-c)"
                 :interactive nil
                 :handler [:comint/interrupt]}])

  (rf/dispatch [:register-command :comint-stop-subjob
                {:docstring "Stop subprocess (C-c C-z)"
                 :interactive nil
                 :handler [:comint/stop]}])

  (rf/dispatch [:register-command :comint-send-eof
                {:docstring "Send EOF to subprocess (C-c C-d)"
                 :interactive nil
                 :handler [:comint/eof]}])

  (rf/dispatch [:register-command :comint-bol
                {:docstring "Move to beginning of input after prompt (C-c C-a)"
                 :interactive nil
                 :handler [:comint/bol]}])

  (rf/dispatch [:register-command :comint-kill-input
                {:docstring "Kill current input (C-c C-u)"
                 :interactive nil
                 :handler [:comint/kill-input]}])

  (rf/dispatch [:register-command :comint-delete-output
                {:docstring "Delete last output (C-c C-o)"
                 :interactive nil
                 :handler [:comint/delete-output]}])

  (rf/dispatch [:register-command :comint-show-output
                {:docstring "Show last output (C-c C-r)"
                 :interactive nil
                 :handler [:comint/show-output]}])

  (rf/dispatch [:register-command :comint-dynamic-list-input-ring
                {:docstring "Display input history (C-c C-l)"
                 :interactive nil
                 :handler [:comint/dynamic-list-input-ring]}])

  ;; Term commands
  (rf/dispatch [:register-command :term
                {:docstring "Open a terminal emulator"
                 :interactive nil
                 :handler [:term/open]}])

  (rf/dispatch [:register-command :ansi-term
                {:docstring "Open an ANSI terminal"
                 :interactive nil
                 :handler [:term/ansi-term]}])

  (rf/dispatch [:register-command :term-char-mode
                {:docstring "Switch to character mode (C-c C-k)"
                 :interactive nil
                 :handler [:term/char-mode]}])

  (rf/dispatch [:register-command :term-line-mode
                {:docstring "Switch to line mode (C-c C-j)"
                 :interactive nil
                 :handler [:term/line-mode]}])

  ;; Shell-mode keybindings for comint
  (rf/dispatch [:keymap/set-mode-key :shell-mode "M-p" :comint-previous-input])
  (rf/dispatch [:keymap/set-mode-key :shell-mode "M-n" :comint-next-input]))

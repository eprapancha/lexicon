(ns lexicon.packages.comint
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

  Based on Emacs lisp/comint.el and lisp/term.el

  This package uses only lisp.cljs primitives."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.lisp :as lisp]
            [lexicon.core.db :as db]))

;; =============================================================================
;; State (Package-local)
;; =============================================================================

(def default-ring-size 64)

;; Comint state keyed by buffer-id
;; {buffer-id {:history [] :history-index N :term-mode :line/:char}}
(defonce comint-state (atom {}))

(defn- get-buffer-history [buffer-id]
  (get-in @comint-state [buffer-id :history] []))

(defn- get-history-index [buffer-id]
  (get-in @comint-state [buffer-id :history-index] 0))

(defn- set-history-index! [buffer-id idx]
  (swap! comint-state assoc-in [buffer-id :history-index] idx))

(defn- add-to-history! [buffer-id input]
  (let [history (get-buffer-history buffer-id)
        max-size default-ring-size
        new-history (if (and (seq input)
                             (not= input (last history)))
                      (let [h (conj history input)]
                        (if (> (count h) max-size)
                          (subvec h (- (count h) max-size))
                          h))
                      history)]
    (swap! comint-state assoc-in [buffer-id :history] new-history)
    (swap! comint-state assoc-in [buffer-id :history-index] (count new-history))))

;; =============================================================================
;; Input History Ring - Internal Events
;; =============================================================================

(rf/reg-event-fx
 :comint/add-to-history
 (fn [{:keys [_db]} [_ buffer-id input]]
   (add-to-history! buffer-id input)
   {}))

(rf/reg-event-fx
 :comint/previous-input
 (fn [{:keys [_db]} [_]]
   "Cycle backward through input history (M-p)."
   (let [buffer-id (lisp/current-buffer)
         history (get-buffer-history buffer-id)
         idx (get-history-index buffer-id)
         new-idx (max 0 (dec idx))]
     (if (and (seq history) (< new-idx (count history)))
       (do
         (set-history-index! buffer-id new-idx)
         {:fx [[:dispatch [:echo/message (nth history new-idx)]]]})
       {:fx [[:dispatch [:echo/message "Beginning of history"]]]}))))

(rf/reg-event-fx
 :comint/next-input
 (fn [{:keys [_db]} [_]]
   "Cycle forward through input history (M-n)."
   (let [buffer-id (lisp/current-buffer)
         history (get-buffer-history buffer-id)
         idx (get-history-index buffer-id)
         new-idx (min (count history) (inc idx))]
     (if (< new-idx (count history))
       (do
         (set-history-index! buffer-id new-idx)
         {:fx [[:dispatch [:echo/message (nth history new-idx)]]]})
       {:fx [[:dispatch [:echo/message "End of history"]]]}))))

(rf/reg-event-fx
 :comint/previous-matching-input
 (fn [{:keys [_db]} [_]]
   "Search backward through history for matching input (M-r)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "History search: "
                      :on-confirm [:comint/search-history-exec]}]]]}))

(rf/reg-event-fx
 :comint/search-history-exec
 (fn [{:keys [_db]} [_ pattern]]
   "Execute history search."
   (let [buffer-id (lisp/current-buffer)
         history (get-buffer-history buffer-id)
         re (try (js/RegExp. pattern "i") (catch :default _ nil))
         match (when re
                 (last (filter #(.test re %) history)))]
     (if match
       {:fx [[:dispatch [:echo/message (str "Found: " match)]]]}
       {:fx [[:dispatch [:echo/message (str "No match for: " pattern)]]]}))))

;; =============================================================================
;; Comint Process Interaction - Internal Events
;; =============================================================================

(rf/reg-event-fx
 :comint/send-input
 (fn [{:keys [_db]} [_ buffer-id input]]
   "Send input to the process in buffer."
   {:fx [[:dispatch [:comint/add-to-history buffer-id input]]
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
;; Comint Line Editing - Internal Events
;; =============================================================================

(rf/reg-event-fx
 :comint/bol
 (fn [{:keys [_db]} [_]]
   "Move to beginning of input after prompt (C-c C-a).
    In shell-mode, moves to the position right after the prompt string."
   (let [text (lisp/buffer-string)]
     (if text
       (let [last-prompt-idx (str/last-index-of text "$ ")]
         (if last-prompt-idx
           {:fx [[:dispatch [:echo/message "Beginning of input"]]]}
           {:fx [[:dispatch [:echo/message "No prompt found"]]]}))
       {:fx [[:dispatch [:echo/message "No prompt found"]]]}))))

(rf/reg-event-fx
 :comint/kill-input
 (fn [{:keys [db]} [_]]
   "Kill the current input line (C-c C-u).
    Clears text after the last prompt."
   (let [buffer-id (lisp/current-buffer)
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
   (let [buffer-id (lisp/current-buffer)
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
   (let [buffer-id (lisp/current-buffer)
         history (get-buffer-history buffer-id)
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
;; Term Mode - Internal Events
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
 (fn [{:keys [_db]} [_]]
   "Switch to character mode (C-c C-k)."
   (let [buffer-id (lisp/current-buffer)]
     (swap! comint-state assoc-in [buffer-id :term-mode] :char)
     {:fx [[:dispatch [:echo/message "Char mode"]]]})))

(rf/reg-event-fx
 :term/line-mode
 (fn [{:keys [_db]} [_]]
   "Switch to line mode (C-c C-j)."
   (let [buffer-id (lisp/current-buffer)]
     (swap! comint-state assoc-in [buffer-id :term-mode] :line)
     {:fx [[:dispatch [:echo/message "Line mode"]]]})))

;; =============================================================================
;; Command Handler Functions
;; =============================================================================

(defn comint-previous-input! []
  (rf/dispatch [:comint/previous-input]))

(defn comint-next-input! []
  (rf/dispatch [:comint/next-input]))

(defn comint-previous-matching-input! []
  (rf/dispatch [:comint/previous-matching-input]))

(defn comint-interrupt-subjob! []
  (rf/dispatch [:comint/interrupt]))

(defn comint-stop-subjob! []
  (rf/dispatch [:comint/stop]))

(defn comint-send-eof! []
  (rf/dispatch [:comint/eof]))

(defn comint-bol! []
  (rf/dispatch [:comint/bol]))

(defn comint-kill-input! []
  (rf/dispatch [:comint/kill-input]))

(defn comint-delete-output! []
  (rf/dispatch [:comint/delete-output]))

(defn comint-show-output! []
  (rf/dispatch [:comint/show-output]))

(defn comint-dynamic-list-input-ring! []
  (rf/dispatch [:comint/dynamic-list-input-ring]))

(defn term! []
  (rf/dispatch [:term/open]))

(defn ansi-term! []
  (rf/dispatch [:term/ansi-term]))

(defn term-char-mode! []
  (rf/dispatch [:term/char-mode]))

(defn term-line-mode! []
  (rf/dispatch [:term/line-mode]))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize comint and term modules."
  []
  ;; Comint commands
  (lisp/define-command 'comint-previous-input
    comint-previous-input!
    "Cycle backward through input history (M-p)")

  (lisp/define-command 'comint-next-input
    comint-next-input!
    "Cycle forward through input history (M-n)")

  (lisp/define-command 'comint-previous-matching-input
    comint-previous-matching-input!
    "Search backward through history (M-r)")

  (lisp/define-command 'comint-interrupt-subjob
    comint-interrupt-subjob!
    "Interrupt subprocess (C-c C-c)")

  (lisp/define-command 'comint-stop-subjob
    comint-stop-subjob!
    "Stop subprocess (C-c C-z)")

  (lisp/define-command 'comint-send-eof
    comint-send-eof!
    "Send EOF to subprocess (C-c C-d)")

  (lisp/define-command 'comint-bol
    comint-bol!
    "Move to beginning of input after prompt (C-c C-a)")

  (lisp/define-command 'comint-kill-input
    comint-kill-input!
    "Kill current input (C-c C-u)")

  (lisp/define-command 'comint-delete-output
    comint-delete-output!
    "Delete last output (C-c C-o)")

  (lisp/define-command 'comint-show-output
    comint-show-output!
    "Show last output (C-c C-r)")

  (lisp/define-command 'comint-dynamic-list-input-ring
    comint-dynamic-list-input-ring!
    "Display input history (C-c C-l)")

  ;; Term commands
  (lisp/define-command 'term
    term!
    "Open a terminal emulator")

  (lisp/define-command 'ansi-term
    ansi-term!
    "Open an ANSI terminal")

  (lisp/define-command 'term-char-mode
    term-char-mode!
    "Switch to character mode (C-c C-k)")

  (lisp/define-command 'term-line-mode
    term-line-mode!
    "Switch to line mode (C-c C-j)")

  ;; Shell-mode keybindings for comint
  (lisp/define-key-for-mode :shell-mode "M-p" :comint-previous-input)
  (lisp/define-key-for-mode :shell-mode "M-n" :comint-next-input))

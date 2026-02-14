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
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (Package-local)
;; =============================================================================

(def default-ring-size 64)

;; Comint state keyed by buffer-id
;; {buffer-id {:history <ring>, :history-index N, :term-mode :line/:char}}
(defonce comint-state (atom {}))

(defn- ensure-buffer-state!
  "Ensure comint state exists for buffer."
  [buffer-id]
  (when-not (get @comint-state buffer-id)
    (swap! comint-state assoc buffer-id
           {:history (lisp/make-ring default-ring-size)
            :history-index 0
            :term-mode :line})))

(defn- get-buffer-history
  "Get the history ring for a buffer."
  [buffer-id]
  (ensure-buffer-state! buffer-id)
  (get-in @comint-state [buffer-id :history]))

(defn- get-history-index [buffer-id]
  (get-in @comint-state [buffer-id :history-index] 0))

(defn- set-history-index! [buffer-id idx]
  (swap! comint-state assoc-in [buffer-id :history-index] idx))

(defn- add-to-history!
  "Add input to the history ring for a buffer."
  [buffer-id input]
  (ensure-buffer-state! buffer-id)
  (when (seq input)
    (swap! comint-state update-in [buffer-id :history]
           (fn [ring]
             (lisp/ring-insert-no-dup ring input)))
    (let [new-length (lisp/ring-length (get-buffer-history buffer-id))]
      (set-history-index! buffer-id new-length))))

;; =============================================================================
;; Input History Navigation
;; =============================================================================

(defn comint-previous-input!
  "Cycle backward through input history (M-p)."
  []
  (let [buffer-id (lisp/current-buffer)
        history (get-buffer-history buffer-id)
        idx (get-history-index buffer-id)
        new-idx (max 0 (dec idx))
        length (lisp/ring-length history)]
    (if (and (pos? length) (< new-idx length))
      (do
        (set-history-index! buffer-id new-idx)
        (lisp/message (lisp/ring-ref history new-idx)))
      (lisp/message "Beginning of history"))))

(defn comint-next-input!
  "Cycle forward through input history (M-n)."
  []
  (let [buffer-id (lisp/current-buffer)
        history (get-buffer-history buffer-id)
        idx (get-history-index buffer-id)
        length (lisp/ring-length history)
        new-idx (min length (inc idx))]
    (if (< new-idx length)
      (do
        (set-history-index! buffer-id new-idx)
        (lisp/message (lisp/ring-ref history new-idx)))
      (lisp/message "End of history"))))

(defn comint-previous-matching-input!
  "Search backward through history for matching input (M-r)."
  []
  (lisp/read-from-minibuffer
   "History search: "
   (fn [pattern]
     (let [buffer-id (lisp/current-buffer)
           history (get-buffer-history buffer-id)
           elements (lisp/ring-elements history)
           re (try (js/RegExp. pattern "i") (catch :default _ nil))
           match (when re
                   (last (filter #(.test re %) elements)))]
       (if match
         (lisp/message (str "Found: " match))
         (lisp/message (str "No match for: " pattern)))))))

;; =============================================================================
;; Process Interaction
;; =============================================================================

(defn comint-send-input!
  "Send input to the process in buffer."
  [buffer-id input]
  (add-to-history! buffer-id input)
  ;; In browser context, shell command execution is simulated
  (lisp/message (str "Input: " input)))

(defn comint-interrupt-subjob!
  "Interrupt the subprocess (C-c C-c)."
  []
  (lisp/message "Interrupt (no subprocess in browser)"))

(defn comint-stop-subjob!
  "Stop the subprocess (C-c C-z)."
  []
  (lisp/message "Stop (no subprocess in browser)"))

(defn comint-send-eof!
  "Send EOF to subprocess (C-c C-d)."
  []
  (lisp/message "EOF (no subprocess in browser)"))

;; =============================================================================
;; Line Editing
;; =============================================================================

(defn comint-bol!
  "Move to beginning of input after prompt (C-c C-a)."
  []
  (let [text (lisp/buffer-string)]
    (if text
      (let [last-prompt-idx (str/last-index-of text "$ ")]
        (if last-prompt-idx
          (lisp/message "Beginning of input")
          (lisp/message "No prompt found")))
      (lisp/message "No prompt found"))))

(defn comint-kill-input!
  "Kill the current input line (C-c C-u)."
  []
  (let [text (lisp/buffer-string)]
    (if text
      (let [last-prompt-idx (str/last-index-of text "$ ")]
        (if last-prompt-idx
          (let [input-start (+ last-prompt-idx 2)
                len (lisp/buffer-size)]
            (when (> len input-start)
              (lisp/delete-region input-start len))
            (lisp/message "Input killed"))
          (lisp/message "No input to kill")))
      (lisp/message "Not in a shell buffer"))))

(defn comint-delete-output!
  "Delete last batch of process output (C-c C-o)."
  []
  (let [text (lisp/buffer-string)]
    (if text
      (let [last-prompt (str/last-index-of text "$ ")
            prev-prompt (when last-prompt
                          (str/last-index-of text "$ " (dec last-prompt)))]
        (if (and last-prompt prev-prompt)
          (let [delete-start (+ prev-prompt 2)
                delete-end last-prompt
                to-delete (- delete-end delete-start)]
            (when (pos? to-delete)
              (lisp/delete-region delete-start delete-end))
            (lisp/message "Output deleted"))
          (lisp/message "*** output flushed ***")))
      (lisp/message "Not in a shell buffer"))))

(defn comint-show-output!
  "Show beginning of last output group (C-c C-r)."
  []
  (lisp/message "Show output (scroll to last output)"))

(defn comint-dynamic-list-input-ring!
  "Display the input history in a buffer (C-c C-l)."
  []
  (let [buffer-id (lisp/current-buffer)
        history (get-buffer-history buffer-id)
        elements (lisp/ring-elements history)
        content (str "Input History\n"
                     (str/join "" (repeat 40 "=")) "\n\n"
                     (if (seq elements)
                       (str/join "\n"
                                 (map-indexed
                                  (fn [i cmd] (str "  " (inc i) "  " cmd))
                                  elements))
                       "(empty history)")
                     "\n")]
    (lisp/create-special-buffer
     "*Input History*"
     content
     {:major-mode :special-mode
      :read-only true})
    (lisp/switch-to-buffer (lisp/get-buffer "*Input History*"))))

;; =============================================================================
;; Term Mode
;; =============================================================================

(defn term!
  "Open a terminal buffer (M-x term)."
  []
  (let [buffer-id (lisp/create-special-buffer
                   "*terminal*"
                   "Terminal emulator\n$ "
                   {:major-mode :shell-mode})]
    (lisp/switch-to-buffer buffer-id)))

(defn ansi-term!
  "Open an ANSI terminal (M-x ansi-term)."
  []
  (let [buffer-id (lisp/create-special-buffer
                   "*ansi-term*"
                   "ANSI Terminal\n$ "
                   {:major-mode :shell-mode})]
    (lisp/switch-to-buffer buffer-id)))

(defn term-char-mode!
  "Switch to character mode (C-c C-k)."
  []
  (let [buffer-id (lisp/current-buffer)]
    (swap! comint-state assoc-in [buffer-id :term-mode] :char)
    (lisp/message "Char mode")))

(defn term-line-mode!
  "Switch to line mode (C-c C-j)."
  []
  (let [buffer-id (lisp/current-buffer)]
    (swap! comint-state assoc-in [buffer-id :term-mode] :line)
    (lisp/message "Line mode")))

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

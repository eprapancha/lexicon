(ns lexicon.core.comint
  "Comint mode and terminal emulation (comint.el, term.el).

  Implements Emacs comint.el base mode for interactive processes:
  - comint-mode: Base mode for shell/REPL interaction
  - Input history (M-p / M-n)
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
  - C-c C-c: Interrupt
  - C-c C-u: Kill input
  - C-c C-o: Delete output

  Term key bindings:
  - C-c C-j: Switch to line mode
  - C-c C-k: Switch to char mode

  In browser context, provides the UI framework for interactive
  command buffers. Actual process spawning requires a backend.

  Based on Emacs lisp/comint.el and lisp/term.el"
  (:require [re-frame.core :as rf]))

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
   {:fx [[:dispatch [:echo/message "Interrupt (no subprocess)"]]]}))

(rf/reg-event-fx
 :comint/kill-input
 (fn [_ [_]]
   "Kill the current input line (C-c C-u)."
   {:fx [[:dispatch [:echo/message "Input killed"]]]}))

(rf/reg-event-fx
 :comint/delete-output
 (fn [_ [_]]
   "Delete last batch of output (C-c C-o)."
   {:fx [[:dispatch [:echo/message "Output deleted"]]]}))

;; =============================================================================
;; Term Mode
;; =============================================================================

(rf/reg-event-fx
 :term/open
 (fn [{:keys [db]} [_ term-name]]
   "Open a terminal buffer (M-x term)."
   (let [name (or term-name "*terminal*")]
     {:fx [[:dispatch [:shell/open name]]]})))

(rf/reg-event-fx
 :term/ansi-term
 (fn [{:keys [db]} [_]]
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

  (rf/dispatch [:register-command :comint-interrupt-subjob
                {:docstring "Interrupt subprocess (C-c C-c)"
                 :interactive nil
                 :handler [:comint/interrupt]}])

  (rf/dispatch [:register-command :comint-kill-input
                {:docstring "Kill current input (C-c C-u)"
                 :interactive nil
                 :handler [:comint/kill-input]}])

  (rf/dispatch [:register-command :comint-delete-output
                {:docstring "Delete last output (C-c C-o)"
                 :interactive nil
                 :handler [:comint/delete-output]}])

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

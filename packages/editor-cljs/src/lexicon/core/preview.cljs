(ns lexicon.core.preview
  "Preview framework for consult-style completion.

  Implements the state function protocol used by consult:
  - (state-fn 'setup nil)    — after minibuffer init
  - (state-fn 'preview cand) — show preview (repeated)
  - (state-fn 'exit nil)     — before minibuffer exit
  - (state-fn 'return cand)  — after exit, final action

  Provides built-in preview constructors:
  - make-jump-preview: save/restore point + window config, goto candidate
  - make-buffer-preview: save/restore window config, show candidate buffer
  - make-insertion-preview: overlay showing candidate text at point

  Preview constructors accept an API map of functions to avoid circular
  dependencies with lexicon.lisp."
  (:require [lexicon.core.log :as log]))

;; =============================================================================
;; State Function Protocol
;; =============================================================================

(defn make-jump-preview
  "Create a jump preview state function.

  On preview: save current point and window config, move cursor to
  candidate position, highlight current line with overlay.
  On cancel (exit without return): restore original position.
  On confirm (return): keep the position.

  GOTO-FN is a function that takes a candidate string and navigates
  to that position (e.g., parses line:col and calls goto-char).
  API-FNS is a map with keys :point, :goto-char,
  :current-window-configuration, :set-window-configuration.

  Returns a state function suitable for consult's :state parameter."
  [goto-fn api-fns]
  (let [saved-state (atom nil)]
    (fn [action candidate]
      (case action
        setup
        (reset! saved-state
                {:window-config (when-let [f (:current-window-configuration api-fns)]
                                  (f))
                 :point (when-let [f (:point api-fns)]
                          (f))})

        preview
        (when (and candidate goto-fn)
          (try
            (goto-fn candidate)
            (catch :default e
              (log/warn (str "Jump preview error: " e)))))

        exit
        (when-let [state @saved-state]
          (when-let [restore-fn (:set-window-configuration api-fns)]
            (when (:window-config state)
              (restore-fn (:window-config state))))
          (when-let [goto (:goto-char api-fns)]
            (when (:point state)
              (goto (:point state)))))

        return
        (when (and candidate goto-fn)
          (try
            (goto-fn candidate)
            (catch :default e
              (log/warn (str "Jump preview return error: " e)))))

        ;; Unknown action, ignore
        nil))))

(defn make-buffer-preview
  "Create a buffer preview state function.

  On preview: save window config, switch window to candidate buffer.
  On cancel: restore original window config.
  On confirm: keep the buffer displayed.

  BUFFER-FN is a function that takes a candidate string and returns
  a buffer-id or buffer name to display.
  API-FNS is a map with keys :current-window-configuration,
  :set-window-configuration, :set-window-buffer,
  :minibuffer-selected-window.

  Returns a state function."
  [buffer-fn api-fns]
  (let [saved-state (atom nil)]
    (fn [action candidate]
      (case action
        setup
        (reset! saved-state
                {:window-config (when-let [f (:current-window-configuration api-fns)]
                                  (f))
                 :original-window (when-let [f (:minibuffer-selected-window api-fns)]
                                    (f))})

        preview
        (when (and candidate buffer-fn)
          (try
            (let [buffer (buffer-fn candidate)]
              (when (and buffer (:set-window-buffer api-fns) (:original-window @saved-state))
                ((:set-window-buffer api-fns) (:original-window @saved-state) buffer)))
            (catch :default e
              (log/warn (str "Buffer preview error: " e)))))

        exit
        (when-let [state @saved-state]
          (when-let [restore-fn (:set-window-configuration api-fns)]
            (when (:window-config state)
              (restore-fn (:window-config state)))))

        return
        (when (and candidate buffer-fn)
          (try
            (let [buffer (buffer-fn candidate)]
              (when (and buffer (:set-window-buffer api-fns) (:original-window @saved-state))
                ((:set-window-buffer api-fns) (:original-window @saved-state) buffer)))
            (catch :default e
              (log/warn (str "Buffer preview return error: " e)))))

        nil))))

(defn make-insertion-preview
  "Create an insertion preview state function.

  On preview: show candidate text at point using an overlay.
  On exit/cancel: remove the overlay.
  On confirm: the overlay is removed (actual insertion handled by command).

  API-FNS is a map with keys :point, :make-overlay, :overlay-put,
  :delete-overlay.

  Returns a state function."
  [api-fns]
  (let [preview-overlay (atom nil)]
    (fn [action candidate]
      (case action
        setup
        nil ;; Nothing to do on setup

        preview
        (when candidate
          (try
            ;; Remove previous preview overlay
            (when-let [ov @preview-overlay]
              (when-let [del-fn (:delete-overlay api-fns)]
                (del-fn ov)))
            ;; Create new overlay at point showing candidate
            (when (and (:make-overlay api-fns) (:overlay-put api-fns) (:point api-fns))
              (let [pos ((:point api-fns))
                    ov ((:make-overlay api-fns) pos pos)]
                ((:overlay-put api-fns) ov :after-string candidate)
                ((:overlay-put api-fns) ov :face :shadow)
                (reset! preview-overlay ov)))
            (catch :default e
              (log/warn (str "Insertion preview error: " e)))))

        exit
        (when-let [ov @preview-overlay]
          (when-let [del-fn (:delete-overlay api-fns)]
            (del-fn ov))
          (reset! preview-overlay nil))

        return
        (when-let [ov @preview-overlay]
          (when-let [del-fn (:delete-overlay api-fns)]
            (del-fn ov))
          (reset! preview-overlay nil))

        nil))))

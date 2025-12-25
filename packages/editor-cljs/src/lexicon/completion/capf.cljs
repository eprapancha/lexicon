(ns lexicon.completion.capf
  "Completion-at-point functions (CAPFs) for in-buffer completion.

  CAPFs provide context-sensitive completions at the current point.
  Each CAPF is a function that:
  1. Checks if completion is appropriate at point
  2. Returns completion data: {:start :end :collection :metadata}
  3. Optionally provides custom behavior (:exit-function, :company-prefix-length)

  The completion-at-point command (TAB) tries CAPFs in order from
  the buffer-local completion-at-point-functions hook until one succeeds."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.completion.tables :as tables]
            [lexicon.completion.metadata :as metadata]))

;; -- CAPF Protocol --

(defn capf-result
  "Create a CAPF result map.

  Required:
  - start: Start position in buffer
  - end: End position in buffer
  - collection: Completion table, vector of candidates, or function

  Optional:
  - metadata: Completion metadata (category, annotations, etc.)
  - exit-function: (candidate status) called after insertion
  - annotation-function: (candidate) -> annotation string
  - company-prefix-length: Prefix length for company-mode compat
  - predicate: (candidate) -> boolean to filter candidates
  - exclusive: If :no, try next CAPF even if this one succeeds

  Returns: Map with CAPF data or nil if completion not applicable"
  [{:keys [start end collection metadata exit-function annotation-function
           company-prefix-length predicate exclusive]
    :or {exclusive true}}]
  {:start start
   :end end
   :collection collection
   :metadata metadata
   :exit-function exit-function
   :annotation-function annotation-function
   :company-prefix-length company-prefix-length
   :predicate predicate
   :exclusive exclusive})

;; -- CAPF Utilities --

(defn buffer-substring
  "Get substring from buffer between START and END positions."
  [buffer-state start end]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try
                 (.getText wasm)
                 (catch js/Error _e ""))
               "")]
    (subs text start (min end (count text)))))

(defn looking-back-at
  "Check if text before POINT matches REGEXP."
  [buffer-state point regexp]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try
                 (.getText wasm)
                 (catch js/Error _e ""))
               "")
        before-text (subs text 0 (min point (count text)))]
    (re-find regexp before-text)))

(defn bounds-of-symbol-at-point
  "Find symbol boundaries at POINT in BUFFER-STATE.
  Returns [start end] or nil if not on a symbol."
  [buffer-state point]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try
                 (.getText wasm)
                 (catch js/Error _e ""))
               "")]
    (when (pos? (count text))
      (let [;; Find start: search backward for non-symbol char
            start (loop [i (dec point)]
                    (cond
                      (< i 0) 0
                      (re-matches #"[a-zA-Z0-9\-_]" (str (nth text i)))
                      (recur (dec i))
                      :else
                      (inc i)))
            ;; Find end: search forward for non-symbol char
            end (loop [i point]
                  (cond
                    (>= i (count text)) (count text)
                    (re-matches #"[a-zA-Z0-9\-_]" (str (nth text i)))
                    (recur (inc i))
                    :else
                    i))]
        (when (< start end)
          [start end])))))

(defn bounds-of-filename-at-point
  "Find filename boundaries at POINT in BUFFER-STATE.
  Returns [start end] or nil if not on a filename."
  [buffer-state point]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try
                 (.getText wasm)
                 (catch js/Error _e ""))
               "")]
    (when (pos? (count text))
      (let [;; File names: alphanumeric + / . - _ ~
            start (loop [i (dec point)]
                    (cond
                      (< i 0) 0
                      (re-matches #"[a-zA-Z0-9/.\-_~]" (str (nth text i)))
                      (recur (dec i))
                      :else
                      (inc i)))
            end (loop [i point]
                  (cond
                    (>= i (count text)) (count text)
                    (re-matches #"[a-zA-Z0-9/.\-_~]" (str (nth text i)))
                    (recur (inc i))
                    :else
                    i))]
        (when (< start end)
          [start end])))))

;; -- Built-in CAPFs --

;; Elisp symbol completion (for *scratch* and Elisp buffers)
(defn elisp-completion-at-point
  "CAPF for Elisp symbols (commands, faces, variables, etc.).
  Active in *scratch* and elisp-mode buffers."
  [db buffer-id]
  (let [buffer (get-in db [:buffers buffer-id])
        point (or (:point buffer) 0)]
    (when-let [[start end] (bounds-of-symbol-at-point buffer point)]
      (let [table (tables/symbol-table db)]
        (capf-result {:start start
                      :end end
                      :collection table
                      :metadata (tables/get-metadata table)})))))

;; File name completion
(defn file-name-completion-at-point
  "CAPF for file paths. Activates when point is on a filename pattern."
  [db buffer-id]
  (let [buffer (get-in db [:buffers buffer-id])
        point (or (:point buffer) 0)]
    (when-let [[start end] (bounds-of-filename-at-point buffer point)]
      ;; For now, return empty - would integrate with file system
      (let [candidates []
            metadata (metadata/make-metadata :category :file)]
        (capf-result {:start start
                      :end end
                      :collection candidates
                      :metadata metadata})))))

;; Dabbrev-style completion (words from current buffer)
(defn dabbrev-completion-at-point
  "CAPF for dynamic abbreviations (words from current buffer).
  Fallback completion when no other CAPF applies."
  [db buffer-id]
  (let [buffer (get-in db [:buffers buffer-id])
        point (or (:point buffer) 0)]
    (when-let [[start end] (bounds-of-symbol-at-point buffer point)]
      (let [wasm (:wasm-instance buffer)
            text (if wasm
                   (try (.getText wasm) (catch js/Error _e ""))
                   "")
            ;; Extract all words from buffer
            words (set (re-seq #"[a-zA-Z0-9\-_]+" text))
            candidates (vec words)
            metadata (metadata/make-metadata :category :symbol)]
        (capf-result {:start start
                      :end end
                      :collection candidates
                      :metadata metadata
                      :exclusive :no})))))  ; Try next CAPF even if this succeeds

;; -- CAPF Hook System --

(defn default-capf-list
  "Default completion-at-point-functions for a buffer.
  Modes can override this by setting buffer-local value."
  []
  [elisp-completion-at-point
   file-name-completion-at-point
   dabbrev-completion-at-point])

(defn get-capf-list
  "Get completion-at-point-functions for BUFFER-ID.
  Checks buffer-local setting, falls back to default."
  [db buffer-id]
  (or (get-in db [:buffers buffer-id :completion-at-point-functions])
      (default-capf-list)))

(defn run-capfs
  "Run CAPFs in order until one returns a result.

  Args:
  - db: Application database
  - buffer-id: Current buffer ID
  - capf-list: List of CAPF functions

  Returns: CAPF result map or nil"
  [db buffer-id capf-list]
  (loop [capfs capf-list]
    (when (seq capfs)
      (let [capf (first capfs)
            result (try
                     (capf db buffer-id)
                     (catch js/Error e
                       (js/console.warn "CAPF error:" e)
                       nil))]
        (if (and result
                 (not= (:exclusive result) :no))
          ;; Found exclusive result, stop
          result
          ;; Try next CAPF
          (recur (rest capfs)))))))

;; -- Completion-at-Point Command --

(defn completion-at-point-candidates
  "Get completion candidates at point in BUFFER-ID.

  Returns: {:candidates [...] :metadata {...} :start N :end N}
          or nil if no completions available"
  [db buffer-id]
  (let [capf-list (get-capf-list db buffer-id)]
    (when-let [result (run-capfs db buffer-id capf-list)]
      (let [{:keys [start end collection metadata]} result
            ;; Get candidates from collection
            candidates (cond
                         ;; CompletionTable
                         (satisfies? tables/CompletionTable collection)
                         (tables/all-completions collection)

                         ;; Vector of candidates
                         (vector? collection)
                         collection

                         ;; Function
                         (fn? collection)
                         (collection nil)

                         :else
                         [])]
        {:candidates candidates
         :metadata (or metadata (when (satisfies? tables/CompletionTable collection)
                                 (tables/get-metadata collection)))
         :start start
         :end end
         :exit-function (:exit-function result)}))))

;; -- Re-frame Events --

;; Set completion-at-point-functions for current buffer
(rf/reg-event-db
  :capf/set-functions
  (fn [db [_ buffer-id capf-list]]
    (assoc-in db [:buffers buffer-id :completion-at-point-functions] capf-list)))

;; Get completion-at-point-functions for buffer
(rf/reg-sub
  :capf/functions
  (fn [db [_ buffer-id]]
    (get-capf-list db buffer-id)))

;; Completion-at-point command (TAB in buffer)
(rf/reg-event-fx
  :completion-at-point
  (fn [{:keys [db]} [_]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])]
      (if-let [completion-data (completion-at-point-candidates db buffer-id)]
        (let [{:keys [candidates metadata start end exit-function]} completion-data
              buffer (get-in db [:buffers buffer-id])
              point (or (:point buffer) 0)
              wasm (:wasm-instance buffer)
              ;; Get the text being completed
              prefix (when wasm
                       (try
                         (let [text (.getText wasm)]
                           (subs text start (min end (count text))))
                         (catch js/Error _e "")))]
          (cond
            ;; No candidates
            (empty? candidates)
            {:db (assoc db :message "No completions available")}

            ;; Single candidate - insert it
            (= 1 (count candidates))
            (let [candidate (first candidates)]
              {:fx [[:dispatch [:buffer/delete-region buffer-id start end]]
                    [:dispatch [:buffer/insert buffer-id start candidate]]
                    ;; Call exit-function if provided
                    (when exit-function
                      [:dispatch [:capf/call-exit-function buffer-id candidate :finished]])]})

            ;; Multiple candidates - show in minibuffer
            :else
            {:fx [[:dispatch [:minibuffer/activate
                              {:prompt "Complete: "
                               :completions candidates
                               :metadata metadata
                               :initial-input prefix
                               :on-confirm [:capf/insert-completion buffer-id start end]}]]]}))
        ;; No CAPF returned results
        {:db (assoc db :message "No completions available")}))))

;; Insert completion from minibuffer
(rf/reg-event-fx
  :capf/insert-completion
  (fn [{:keys [db]} [_ buffer-id start end completion]]
    {:fx [[:dispatch [:buffer/delete-region buffer-id start end]]
          [:dispatch [:buffer/insert buffer-id start completion]]]}))

;; Call CAPF exit-function
(rf/reg-event-fx
  :capf/call-exit-function
  (fn [{:keys [db]} [_ buffer-id candidate status]]
    ;; Status can be: :finished, :sole, :exact
    ;; For now, just log - modes can override
    (js/console.log "CAPF exit:" candidate status)
    {}))

;; -- Mode Integration --

;; Helper to set CAPFs for a mode
(defn set-mode-capfs!
  "Set completion-at-point-functions for MODE-NAME.
  Called when mode is activated."
  [mode-name capf-list]
  (rf/dispatch [:capf/set-functions-for-mode mode-name capf-list]))

(rf/reg-event-db
  :capf/set-functions-for-mode
  (fn [db [_ mode-name capf-list]]
    (assoc-in db [:modes mode-name :completion-at-point-functions] capf-list)))

(rf/reg-sub
  :capf/functions-for-mode
  (fn [db [_ mode-name]]
    (get-in db [:modes mode-name :completion-at-point-functions]
            (default-capf-list))))

;; Initialize CAPFs for fundamental-mode
(defn initialize-capfs! []
  ;; Fundamental mode gets all default CAPFs
  (rf/dispatch [:capf/set-functions-for-mode :fundamental-mode (default-capf-list)])

  ;; Lisp modes get symbol completion prioritized
  (rf/dispatch [:capf/set-functions-for-mode :emacs-lisp-mode
                [elisp-completion-at-point
                 dabbrev-completion-at-point]])

  ;; Help mode gets no completion (read-only)
  (rf/dispatch [:capf/set-functions-for-mode :help-mode []])

  ;; Buffer menu mode gets no completion (special buffer)
  (rf/dispatch [:capf/set-functions-for-mode :buffer-menu-mode []]))

;; Auto-initialize on namespace load
(initialize-capfs!)

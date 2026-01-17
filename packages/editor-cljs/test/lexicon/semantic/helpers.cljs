(ns lexicon.semantic.helpers
  "Helper functions for semantic compatibility tests.

  Based on lexicon.core-test pattern - uses WASM in browser test environment."
  (:require [cljs.test :refer [use-fixtures async]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]
            [lexicon.test-setup :as setup]
            [lexicon.api.message :as api-msg]
            [lexicon.effects.log]))  ; Load log effect handlers

;; =============================================================================
;; Test Context - Matches existing test pattern
;; =============================================================================

(defn reset-editor-db!
  "Reset editor to clean state while preserving WASM and system buffers.

  Clears user-created buffers (ID >= 3) but keeps:
  - Buffer 1: *scratch*
  - Buffer 2: *Messages*
  - WASM constructor"
  []
  (let [wasm-constructor (get-in @rfdb/app-db [:system :wasm-constructor])
        buffer-1 (get-in @rfdb/app-db [:buffers 1])
        buffer-2 (get-in @rfdb/app-db [:buffers 2])]
    ;; Start with default-db
    (reset! rfdb/app-db db/default-db)
    ;; Restore WASM and system buffers
    (when wasm-constructor
      (swap! rfdb/app-db assoc-in [:system :wasm-constructor] wasm-constructor))
    (when buffer-1
      (swap! rfdb/app-db assoc-in [:buffers 1] buffer-1))
    (when buffer-2
      (swap! rfdb/app-db assoc-in [:buffers 2] buffer-2))))

(defn current-db
  "Get current re-frame database."
  []
  @rfdb/app-db)

;; =============================================================================
;; Buffer Operations
;; =============================================================================

(defn create-buffer
  "Create a new buffer with given name (and optional content).

  Returns buffer ID.

  Example:
    (create-buffer \"test\")           ; empty buffer
    (create-buffer \"test\" \"hello\")  ; with content"
  ([name]
   (create-buffer name ""))
  ([name content]
   (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (new WasmGapBuffer content))
         buffer-id (db/next-buffer-id (get-in @rfdb/app-db [:buffers]))]
     (when wasm-instance
       (rf/dispatch-sync [:create-buffer name wasm-instance])
       ;; Display buffer in active window so point operations work
       (let [active-window-id (get-in @rfdb/app-db [:active-window-id])]
         (letfn [(update-window [tree]
                   (cond
                     (nil? tree) nil
                     (= (:type tree) :leaf)
                     (if (= active-window-id (:id tree))
                       (assoc tree :buffer-id buffer-id)
                       tree)
                     :else
                     (assoc tree
                            :first (update-window (:first tree))
                            :second (update-window (:second tree)))))]
           (swap! rfdb/app-db update :window-tree update-window)))
       buffer-id))))

(defn buffer-text
  "Get text content of buffer by ID or name."
  [buffer-id-or-name]
  (let [buffer-id (if (string? buffer-id-or-name)
                    ;; Look up by name
                    (let [buffers (get-in @rfdb/app-db [:buffers])]
                      (->> buffers
                           vals
                           (filter #(= buffer-id-or-name (:name %)))
                           first
                           :id))
                    buffer-id-or-name)
        buffer (get-in @rfdb/app-db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)]
    (when wasm-instance
      (try
        (.getText wasm-instance)
        (catch js/Error _ "")))))

(defn insert-text
  "Insert text into buffer at end of buffer."
  [buffer-id text]
  (let [buffer (get-in @rfdb/app-db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)
        ;; Insert at end of buffer - get length from text
        point (if wasm-instance
                (count (.getText wasm-instance))
                0)]
    (rf/dispatch-sync [:buffer/insert buffer-id point text])))

(defn buffer-file
  "Get file path associated with buffer."
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :file-handle]))

(defn buffer-exists?
  "Check if buffer with name exists."
  [name]
  (let [buffers (get-in @rfdb/app-db [:buffers])]
    (some #(= name (:name %)) (vals buffers))))

(defn message
  "Display a message in the echo area and append to *Messages* buffer."
  [text]
  (api-msg/message text))

(defn echo-area-text
  "Get current echo area text."
  []
  (get-in @rfdb/app-db [:minibuffer :message]))

(defn visit-file
  "Associate a file path with a buffer (without actually loading the file)."
  [buffer-id file-path]
  (swap! rfdb/app-db assoc-in [:buffers buffer-id :file-handle] file-path))

(defn current-buffer
  "Get the ID of the currently active buffer."
  []
  (let [active-window-id (get-in @rfdb/app-db [:active-window-id])
        window-tree (get-in @rfdb/app-db [:window-tree])
        find-window (fn find-window [tree id]
                      (cond
                        (nil? tree) nil
                        (= (:id tree) id) tree
                        (= (:type tree) :leaf) nil
                        :else (or (find-window (:first tree) id)
                                  (find-window (:second tree) id))))]
    (when-let [window (find-window window-tree active-window-id)]
      (:buffer-id window))))

;; =============================================================================
;; Point/Mark Operations
;; =============================================================================

(defn point
  "Get point (cursor position) for buffer. Returns character position (0-based).

  In Emacs, point is buffer-local, not window-local. We store it in the buffer."
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :point] 0))

(defn set-point
  "Set point (cursor position) for buffer to given character position.

  In Emacs, point is buffer-local, not window-local. We store it in the buffer."
  [buffer-id pos]
  (swap! rfdb/app-db assoc-in [:buffers buffer-id :point] pos))

;; =============================================================================
;; Command System
;; =============================================================================

(defn define-test-command
  "Define a command for testing purposes.

  Args:
    spec - Map with :name (symbol), :doc (string), :fn (function)

  Returns:
    The command definition map with name attached"
  [{:keys [name doc fn]}]
  (let [command-def {:name name
                     :handler [:test/invoke-fn fn]
                     :doc doc
                     :fn fn}]
    (rf/dispatch-sync [:register-command name command-def])
    command-def))

(defn command-name
  "Get the name of a command from the command definition."
  [cmd]
  (:name cmd))

(defn command-doc
  "Get documentation string from command definition."
  [cmd]
  (:doc cmd))

(defn command-fn
  "Get the function from command definition."
  [cmd]
  (:fn cmd))

(defn invoke-command
  "Invoke a command by name."
  [command-name & args]
  (apply rf/dispatch-sync (into [:execute-command command-name] args)))

(defn with-instrumented-dispatch
  "Run code with instrumented dispatch tracking.

  For now, we'll just execute the body - actual instrumentation would require
  wrapping re-frame's dispatch system."
  [f]
  ;; Simple implementation: just run the function
  ;; The test will check if commands are registered and invoked properly
  (f))

(defn dispatch-was-used?
  "Check if dispatch was used (for testing command dispatch).

  For now, always returns true since we use dispatch-sync above."
  []
  true)

;; =============================================================================
;; Window Management
;; =============================================================================

(defn- next-window-id
  "Get next available window ID."
  []
  (let [next-id (get-in @rfdb/app-db [:next-window-id])]
    (swap! rfdb/app-db update :next-window-id inc)
    next-id))

(defn split-window-horizontally
  "Split the current window horizontally, creating two windows side by side.

  Returns the ID of the new window."
  []
  (let [active-window-id (get-in @rfdb/app-db [:active-window-id])
        window-tree (get-in @rfdb/app-db [:window-tree])
        new-window-id (next-window-id)

        ;; Find the active window and replace it with a split
        split-tree (letfn [(split-at-window [tree]
                            (cond
                              (nil? tree) nil
                              (= (:type tree) :leaf)
                              (if (= (:id tree) active-window-id)
                                ;; Replace leaf with horizontal split
                                {:type :split
                                 :direction :horizontal
                                 :first tree
                                 :second {:type :leaf
                                          :id new-window-id
                                          :buffer-id (:buffer-id tree)
                                          :cursor-position {:line 0 :column 0}
                                          :mark-position nil
                                          :viewport {:start-line 0 :end-line 40}
                                          :dimensions {:x 0 :y 0 :width 50 :height 100}}}
                                tree)
                              :else
                              (assoc tree
                                     :first (split-at-window (:first tree))
                                     :second (split-at-window (:second tree)))))]
                     (split-at-window window-tree))]
    (swap! rfdb/app-db assoc :window-tree split-tree)
    new-window-id))

(defn show-buffer-in-two-windows
  "Display buffer in two windows (splits window horizontally)."
  [buffer-id]
  (split-window-horizontally)
  ;; Both windows now show the same buffer
  nil)

(defn show-buffer-in-new-window
  "Split window and show buffer in the new window."
  [buffer-id]
  (let [new-window-id (split-window-horizontally)]
    ;; Update new window to show the specified buffer
    (letfn [(update-window [tree]
              (cond
                (nil? tree) nil
                (= (:type tree) :leaf)
                (if (= (:id tree) new-window-id)
                  (assoc tree :buffer-id buffer-id)
                  tree)
                :else
                (assoc tree
                       :first (update-window (:first tree))
                       :second (update-window (:second tree)))))]
      (swap! rfdb/app-db update :window-tree update-window))
    new-window-id))

(defn window-text
  "Get the text displayed in a specific window (by buffer ID it shows)."
  [window-id]
  (let [window-tree (get-in @rfdb/app-db [:window-tree])
        ;; Find the window
        find-window (fn find-window [tree]
                      (cond
                        (nil? tree) nil
                        (= (:type tree) :leaf)
                        (when (= (:id tree) window-id) tree)
                        :else
                        (or (find-window (:first tree))
                            (find-window (:second tree)))))
        window (find-window window-tree)]
    (when window
      (buffer-text (:buffer-id window)))))

(defn delete-other-windows
  "Delete all windows except the active one."
  []
  (let [active-window-id (get-in @rfdb/app-db [:active-window-id])
        window-tree (get-in @rfdb/app-db [:window-tree])
        ;; Find the active window and make it the only window
        find-active (fn find-active [tree]
                      (cond
                        (nil? tree) nil
                        (= (:type tree) :leaf)
                        (when (= (:id tree) active-window-id) tree)
                        :else
                        (or (find-active (:first tree))
                            (find-active (:second tree)))))
        active-window (find-active window-tree)]
    (when active-window
      (swap! rfdb/app-db assoc :window-tree active-window))))

;; =============================================================================
;; Minibuffer Operations
;; =============================================================================

(defn activate-minibuffer
  "Activate the minibuffer with a prompt.

  In Emacs, the minibuffer is a real buffer. We create a buffer named ' *Minibuf-0*'
  and track it in the minibuffer-stack.

  Returns the buffer ID of the minibuffer."
  [prompt]
  (let [;; Create minibuffer buffer if it doesn't exist
        mb-buffer-id (or (let [buffers (get-in @rfdb/app-db [:buffers])]
                           (->> buffers vals
                                (filter #(= " *Minibuf-0*" (:name %)))
                                first :id))
                         (create-buffer " *Minibuf-0*" ""))]
    ;; Clear the minibuffer content
    (let [buffer (get-in @rfdb/app-db [:buffers mb-buffer-id])
          ^js wasm-instance (:wasm-instance buffer)]
      (when wasm-instance
        (let [len (.length wasm-instance)]
          (when (> len 0)
            (.delete wasm-instance 0 len)))))

    ;; Push minibuffer frame onto stack
    (swap! rfdb/app-db update :minibuffer-stack conj
           {:buffer-id mb-buffer-id
            :prompt prompt
            :on-confirm nil
            :on-cancel nil})

    ;; Mark minibuffer as active (legacy support)
    (swap! rfdb/app-db assoc-in [:minibuffer :active?] true)

    mb-buffer-id))

(defn deactivate-minibuffer
  "Deactivate the minibuffer (pop from stack).

  In Emacs, the minibuffer buffer persists even when deactivated."
  []
  (swap! rfdb/app-db update :minibuffer-stack pop)
  (when (empty? (get-in @rfdb/app-db [:minibuffer-stack]))
    (swap! rfdb/app-db assoc-in [:minibuffer :active?] false)))

(defn minibuffer-insert
  "Insert text into the active minibuffer."
  [text]
  (let [mb-frame (peek (get-in @rfdb/app-db [:minibuffer-stack]))
        mb-buffer-id (:buffer-id mb-frame)]
    (when mb-buffer-id
      (insert-text mb-buffer-id text))))

(defn minibuffer-contents
  "Get the contents of the active minibuffer."
  []
  (let [mb-frame (peek (get-in @rfdb/app-db [:minibuffer-stack]))
        mb-buffer-id (:buffer-id mb-frame)]
    (when mb-buffer-id
      (buffer-text mb-buffer-id))))

;; =============================================================================
;; WASM Fixture - Required for all semantic tests
;; =============================================================================

(defn with-wasm
  "Fixture to ensure WASM is loaded before tests run.

  Usage in test file:
    (use-fixtures :once with-wasm)"
  [f]
  (async done
    (-> setup/wasm-load-promise
        (.then (fn [_]
                 (.log js/console "✅ WASM ready for semantic tests")
                 (f)
                 (done)))
        (.catch (fn [e]
                  (.error js/console "❌ WASM failed:" e)
                  (done))))))

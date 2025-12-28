(ns lexicon.test-events
  "Test-specific event handlers for testing Lexicon editor.

  These events are simplified versions that bypass normal editor workflows
  to allow direct manipulation of buffers for testing purposes."
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.events :as events]
            [clojure.string]))

;; Buffer manipulation events for tests

(rf/reg-event-db
 :create-buffer
 (fn [db [_ name & rest]]
   "Create a buffer (for testing) - handles both signatures"
   (let [WasmGapBuffer (get-in db [:system :wasm-constructor])]
     (if (keyword? (first rest))
       ;; Signature 2: [:create-buffer name :buffer-id id :content text]  (from test)
       (let [{:keys [buffer-id content]} (apply hash-map rest)
             wasm-instance (when WasmGapBuffer (new WasmGapBuffer (or content "")))]
         (when (and buffer-id wasm-instance)
           (-> db
               (assoc-in [:buffers buffer-id] {:buffer-id buffer-id
                                               :name name
                                               :wasm-instance wasm-instance
                                               :editor-version 0}))))
       ;; Signature 1: [:create-buffer name wasm-instance]
       (let [wasm-instance (first rest)
             buffer-id (db/next-buffer-id (:buffers db))]
         (when wasm-instance
           (-> db
               (assoc-in [:buffers buffer-id] {:buffer-id buffer-id
                                               :name name
                                               :wasm-instance wasm-instance
                                               :editor-version 0}))))))))

(rf/reg-event-db
 :buffer/goto-char
 (fn [db [_ buffer-id pos]]
   "Move point to position POS in BUFFER-ID (for testing)"
   ;; Cursor position is stored in :ui :cursor-position, not in WASM
   (assoc-in db [:ui :cursor-position] pos)))

(rf/reg-event-db
 :buffer/insert
 (fn [db [_ buffer-id pos text]]
   "Insert TEXT at position POS in BUFFER-ID (for testing)"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)]
     (when wasm
       (.insert ^js wasm pos text))
     (update-in db [:buffers buffer-id :editor-version] (fnil inc 0)))))

(rf/reg-event-db
 :buffer/delete
 (fn [db [_ buffer-id start end]]
   "Delete text from START to END in BUFFER-ID (for testing)"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)]
     (when wasm
       ;; WASM delete takes (position, length), not (start, end)
       (.delete ^js wasm start (- end start)))
     (update-in db [:buffers buffer-id :editor-version] (fnil inc 0)))))

(rf/reg-event-db
 :set-current-buffer
 (fn [db [_ buffer-id]]
   "Set the current buffer to BUFFER-ID (for testing)"
   (if (get-in db [:buffers buffer-id])
     (let [active-window-id (:active-window-id db)
           window-tree (:window-tree db)
           new-tree (lexicon.db/update-window-in-tree window-tree active-window-id
                                                      #(assoc % :buffer-id buffer-id))]
       (cond-> db
         true (assoc-in [:editor :current-buffer-id] buffer-id)
         new-tree (assoc :window-tree new-tree)))
     db)))

(rf/reg-event-db
 :self-insert-command
 (fn [db [_ char]]
   "Insert CHAR at point in current buffer (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)]
     (if wasm
       (do
         ;; Insert character at current position
         (.insert ^js wasm point char)
         ;; Query WASM for new length to get accurate UTF-8 byte position
         (let [new-pos (.length wasm)]
           (-> db
               (assoc-in [:ui :cursor-position] new-pos)
               (update-in [:buffers buffer-id :editor-version] (fnil inc 0)))))
       db))))

(rf/reg-event-db
 :newline
 (fn [db [_]]
   "Insert newline at point in current buffer (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)]
     (if wasm
       (do
         ;; Insert newline at current position
         (.insert ^js wasm point "\n")
         ;; Newline is 1 byte in UTF-8
         (-> db
             (assoc-in [:ui :cursor-position] (inc point))
             (update-in [:buffers buffer-id :editor-version] (fnil inc 0))))
       db))))

;; Cursor movement events (synchronous for testing)
(rf/reg-event-db
 :forward-char
 (fn [db [_]]
   "Move cursor forward one character (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         ^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [max-pos (.length wasm-instance)
             new-pos (min max-pos (inc current-pos))]
         (assoc-in db [:ui :cursor-position] new-pos))
       db))))

(rf/reg-event-db
 :backward-char
 (fn [db [_]]
   "Move cursor backward one character (for testing)"
   (let [current-pos (get-in db [:ui :cursor-position] 0)
         new-pos (max 0 (dec current-pos))]
     (assoc-in db [:ui :cursor-position] new-pos))))

(rf/reg-event-db
 :beginning-of-line
 (fn [db [_]]
   "Move to beginning of line (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         ^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             ;; Find start of current line by searching backward for newline
             text-before (subs text 0 current-pos)
             last-newline (.lastIndexOf text-before "\n")
             new-pos (if (= last-newline -1) 0 (inc last-newline))]
         (assoc-in db [:ui :cursor-position] new-pos))
       db))))

(rf/reg-event-db
 :end-of-line
 (fn [db [_]]
   "Move to end of line (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         ^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             ;; Find end of current line by searching forward for newline
             text-after (subs text current-pos)
             next-newline (.indexOf text-after "\n")
             new-pos (if (= next-newline -1)
                       (count text)
                       (+ current-pos next-newline))]
         (assoc-in db [:ui :cursor-position] new-pos))
       db))))

;; Mark and region events
(rf/reg-event-db
 :set-mark-command
 (fn [db [_]]
   "Set mark at current point (for testing)"
   (let [point (get-in db [:ui :cursor-position] 0)]
     (assoc-in db [:ui :mark] point))))

(rf/reg-event-db
 :kill-region
 (fn [db [_]]
   "Kill text between mark and point (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)
         mark (get-in db [:ui :mark] 0)
         start (min mark point)
         end (max mark point)]
     (if wasm
       (let [full-text (.getText wasm)
             killed-text (subs full-text start end)]
         ;; Delete the region - WASM delete takes (position, length)
         (.delete wasm start (- end start))
         ;; Add to kill ring and update state
         (-> db
             (assoc-in [:ui :cursor-position] start)
             (update :kill-ring (fnil conj []) killed-text)
             (update-in [:buffers buffer-id :editor-version] (fnil inc 0))))
       db))))

(rf/reg-event-db
 :yank
 (fn [db [_]]
   "Yank text from kill ring at point (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)
         kill-ring (:kill-ring db)
         text-to-yank (first kill-ring)]
     (if (and wasm text-to-yank)
       (do
         (.insert wasm point text-to-yank)
         (-> db
             (assoc-in [:ui :cursor-position] (+ point (count text-to-yank)))
             (update-in [:buffers buffer-id :editor-version] (fnil inc 0))))
       db))))

(rf/reg-event-db
 :delete-backward-char
 (fn [db [_]]
   "Delete character before point (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)]
     (if (and buffer-id buffer wasm (> point 0))
       (try
         ;; WASM delete takes (position, length), not (start, end)
         (.delete wasm (dec point) 1)
         (-> db
             (assoc-in [:ui :cursor-position] (dec point))
             (update-in [:buffers buffer-id :editor-version] (fnil inc 0)))
         (catch js/Error e
           (.error js/console "delete-backward-char failed:" e)
           db))
       db))))

(rf/reg-event-db
 :undo
 (fn [db [_]]
   "Undo last operation (for testing)"
   ;; TODO: WASM doesn't expose undo API yet
   ;; This is a stub that will fail tests but not error
   (.warn js/console "Undo not yet implemented in WASM")
   db))

(rf/reg-event-db
 :split-window-horizontally
 (fn [db [_]]
   "Split current window horizontally (for testing)"
   (let [current-window-id (get-in db [:editor :active-window-id])
         current-buffer-id (get-in db [:windows current-window-id :buffer-id])
         new-window-id (random-uuid)]
     (-> db
         (assoc-in [:windows new-window-id] {:buffer-id current-buffer-id
                                              :window-id new-window-id})
         (assoc-in [:editor :active-window-id] new-window-id)))))

(rf/reg-event-db
 :split-window-vertically
 (fn [db [_]]
   "Split current window vertically (for testing)"
   (let [current-window-id (get-in db [:editor :active-window-id])
         current-buffer-id (get-in db [:windows current-window-id :buffer-id])
         new-window-id (random-uuid)]
     (-> db
         (assoc-in [:windows new-window-id] {:buffer-id current-buffer-id
                                              :window-id new-window-id})
         (assoc-in [:editor :active-window-id] new-window-id)))))

(rf/reg-event-db
 :other-window
 (fn [db [_]]
   "Switch to another window (for testing)"
   (let [current-window-id (get-in db [:editor :active-window-id])
         all-window-ids (keys (get-in db [:windows]))
         other-windows (remove #(= % current-window-id) all-window-ids)
         next-window-id (first other-windows)]
     (if next-window-id
       (assoc-in db [:editor :active-window-id] next-window-id)
       db))))

(rf/reg-event-db
 :delete-window
 (fn [db [_]]
   "Delete current window (for testing)"
   (let [current-window-id (get-in db [:editor :active-window-id])
         all-window-ids (keys (get-in db [:windows]))
         other-windows (remove #(= % current-window-id) all-window-ids)
         next-window-id (first other-windows)]
     (if (and next-window-id (> (count all-window-ids) 1))
       (-> db
           (update :windows dissoc current-window-id)
           (assoc-in [:editor :active-window-id] next-window-id))
       db))))

(rf/reg-event-db
 :delete-other-windows
 (fn [db [_]]
   "Delete all windows except current (for testing)"
   (let [current-window-id (get-in db [:editor :active-window-id])
         current-window (get-in db [:windows current-window-id])]
     (if current-window
       (assoc db :windows {current-window-id current-window})
       db))))

;; Minibuffer events
(rf/reg-event-db
 :minibuffer/activate
 (fn [db [_ options]]
   "Activate minibuffer with prompt and completions (for testing)"
   (assoc db :minibuffer
          {:active? true
           :prompt (:prompt options)
           :completions (:completions options)
           :on-confirm (:on-confirm options)
           :input ""})))

(rf/reg-event-db
 :minibuffer/complete
 (fn [db [_]]
   "Complete minibuffer input (for testing)"
   (let [minibuffer (:minibuffer db)
         completions (:completions minibuffer)
         input (:input minibuffer "")
         ;; Simple completion: find first match
         match (first (filter #(clojure.string/starts-with? % input) completions))]
     (if match
       (assoc-in db [:minibuffer :input] match)
       db))))

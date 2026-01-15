(ns lexicon.events.edit
  "Event handlers for editing operations, cursor movement, and kill/yank.

  Handles:
  - Text editing (delete-backward-char, delete-forward-char)
  - Cursor movement (forward-char, backward-char, next-line, previous-line, etc.)
  - Cursor management (update-cursor-position, set-cursor-position, initialize-buffer-cursor)
  - Editing operations (kill-line, open-line)
  - Mark and region (set-mark-command, set-mark, copy-region-as-kill, kill-region)
  - Kill ring operations (yank, yank-pop)
  - Undo operations (undo, redo)"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.constants :as const]
            [lexicon.events.buffer :as buffer-events]
            [lexicon.advanced-undo :as undo]))

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
          ;; If we're on non-word char, skip to start of next word, then to end of that word
          (let [word-start (loop [p pos]
                             (if (or (>= p len) (word-char? (get text p)))
                               p
                               (recur (inc p))))]
            ;; Now skip to end of that word
            (loop [p word-start]
              (if (or (>= p len) (not (word-char? (get text p))))
                p
                (recur (inc p))))))))))

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
 (fn [{:keys [db]} [_ & args]]
   "Move cursor forward N characters (C-f or Right arrow)
    With prefix arg, moves that many characters."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)
         ;; Use prefix-arg if available (Phase 6.5)
         prefix-arg (:current-prefix-arg db)
         n (if prefix-arg
             (cond
               (number? prefix-arg) prefix-arg
               (list? prefix-arg) (first prefix-arg)
               :else 1)
             1)]
     (if wasm-instance
       (let [max-pos (.length wasm-instance)
             new-pos (min max-pos (+ current-pos n))]
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
 (fn [{:keys [db]} [_ & args]]
   "Move cursor to next line(s) (C-n or Down arrow)
    With prefix arg, moves that many lines."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)
         ;; Use prefix-arg if available (Phase 6.5)
         prefix-arg (:current-prefix-arg db)
         n (if prefix-arg
             (cond
               (number? prefix-arg) prefix-arg
               (list? prefix-arg) (first prefix-arg)
               :else 1)
             1)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)
             lines (clojure.string/split text #"\n" -1)
             next-line-num (+ line n)]
         (if (< next-line-num (count lines))
           (let [new-pos (buffer-events/line-col-to-linear-pos text next-line-num column)]
             {:fx [[:dispatch [:update-cursor-position new-pos]]]})
           ;; Move to last line if target exceeds buffer
           (let [last-line (dec (count lines))
                 new-pos (buffer-events/line-col-to-linear-pos text last-line column)]
             {:fx [[:dispatch [:update-cursor-position new-pos]]]})))
       {:db db}))))

(rf/reg-event-fx
 :previous-line
 (fn [{:keys [db]} [_ & args]]
   "Move cursor to previous line(s) (C-p or Up arrow)
    With prefix arg, moves that many lines."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)
         ;; Use prefix-arg if available (Phase 6.5)
         prefix-arg (:current-prefix-arg db)
         n (if prefix-arg
             (cond
               (number? prefix-arg) prefix-arg
               (list? prefix-arg) (first prefix-arg)
               :else 1)
             1)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)
             prev-line-num (- line n)]
         (if (>= prev-line-num 0)
           (let [new-pos (buffer-events/line-col-to-linear-pos text prev-line-num column)]
             {:fx [[:dispatch [:update-cursor-position new-pos]]]})
           ;; Move to first line if target is negative
           (let [new-pos (buffer-events/line-col-to-linear-pos text 0 column)]
             {:fx [[:dispatch [:update-cursor-position new-pos]]]})))
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
             new-pos (buffer-events/line-col-to-linear-pos text line column)]
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
             new-pos (buffer-events/line-col-to-linear-pos text line 0)]
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
             new-pos (buffer-events/line-col-to-linear-pos text line line-length)]
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

(rf/reg-event-fx
 :scroll-up-command
 (fn [{:keys [db]} [_]]
   "Scroll forward one screen (C-v)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)
         viewport (:viewport active-window)
         ;; Calculate actual visible lines from viewport
         screen-lines (if viewport
                       (- (:end-line viewport) (:start-line viewport))
                       20)]  ; Fallback to 20 if viewport not set
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)
             target-line (+ line screen-lines)
             new-pos (buffer-events/line-col-to-linear-pos text target-line column)]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

(rf/reg-event-fx
 :scroll-down-command
 (fn [{:keys [db]} [_]]
   "Scroll backward one screen (M-v)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)
         viewport (:viewport active-window)
         ;; Calculate actual visible lines from viewport
         screen-lines (if viewport
                       (- (:end-line viewport) (:start-line viewport))
                       20)]  ; Fallback to 20 if viewport not set
     (if wasm-instance
       (let [text (.getText wasm-instance)
             {:keys [line column]} (linear-pos-to-line-col text current-pos)
             target-line (max 0 (- line screen-lines))
             new-pos (buffer-events/line-col-to-linear-pos text target-line column)]
         {:fx [[:dispatch [:update-cursor-position new-pos]]]})
       {:db db}))))

;; -- Editing Commands --

(rf/reg-event-fx
 :kill-word
 (fn [{:keys [db]} [_]]
   "Kill from cursor to end of word (M-d)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             end-pos (find-forward-word-boundary text current-pos)
             length (- end-pos current-pos)]
         (if (> length 0)
           (let [killed-text (.getRange wasm-instance current-pos end-pos)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))]
             {:db (assoc db :kill-ring updated-kill-ring)
              :fx [[:dispatch [:editor/queue-transaction
                              {:op :delete-range :start current-pos :length length}]]]})
           {:db db}))
       {:db db}))))

(rf/reg-event-fx
 :backward-kill-word
 (fn [{:keys [db]} [_]]
   "Kill from cursor backward to beginning of word (M-DEL)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
         current-pos (get-in db [:ui :cursor-position] 0)]
     (if wasm-instance
       (let [text (.getText wasm-instance)
             start-pos (find-backward-word-boundary text current-pos)
             length (- current-pos start-pos)]
         (if (> length 0)
           (let [killed-text (.getRange wasm-instance start-pos current-pos)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))]
             {:db (assoc db :kill-ring updated-kill-ring)
              :fx [[:dispatch [:editor/queue-transaction
                              {:op :delete-range :start start-pos :length length}]]]})
           {:db db}))
       {:db db}))))

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
                       (buffer-events/line-col-to-linear-pos text line line-length))
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

;; :open-line moved to lexicon.events.command (Phase 6.6)

;; -- Mark and Region Commands --

(rf/reg-event-db
 :set-mark-command
 (fn [db [_]]
   "Set mark at current cursor position (C-SPC)"
   (let [current-pos (get-in db [:ui :cursor-position] 0)
         active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         new-tree (db/update-window-in-tree window-tree active-window-id
                                            #(assoc % :mark-position current-pos))]
     (println "✓ Mark set at position" current-pos)
     (assoc db :window-tree new-tree))))

(rf/reg-event-db
 :set-mark
 (fn [db [_]]
   "Set the mark at the current cursor position"
   (let [cursor-pos (get-in db [:ui :cursor-position])
         active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         new-tree (db/update-window-in-tree window-tree active-window-id
                                            #(assoc % :mark-position cursor-pos))]
     (assoc db :window-tree new-tree))))

(rf/reg-event-db
 :deactivate-mark
 (fn [db [_]]
   "Deactivate the mark (clear region highlighting).

   This is a first-class operation that can be used by any command
   that needs to clear the region, such as query-replace, keyboard-quit, etc."
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         new-tree (db/update-window-in-tree window-tree active-window-id
                                            #(assoc % :mark-position nil))]
     (assoc db :window-tree new-tree))))

(rf/reg-event-db
 :exchange-point-and-mark
 (fn [db [_]]
   "Exchange the position of point and mark (C-x C-x)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)
         mark-position (:mark-position active-window)  ; Linear position
         cursor-pos (get-in db [:ui :cursor-position] 0)]  ; Linear position
     (if mark-position
       (let [buffer-id (:buffer-id active-window)
             buffer-text (get-in db [:buffers buffer-id :cache :text])

             ;; Convert linear mark position to line-col for window cursor
             mark-line-col (buffer-events/linear-to-line-col buffer-text mark-position)

             ;; Sync window cursor to where mark was (Issue #65)
             synced-db (buffer-events/sync-window-and-global-cursor db active-window-id mark-line-col)

             ;; Update mark to where cursor was (cursor-pos is already linear)
             new-tree (db/update-window-in-tree (:window-tree synced-db) active-window-id
                                                #(assoc % :mark-position cursor-pos))]
         (println "✓ Exchanged point and mark:" cursor-pos "<->" mark-position)
         (assoc synced-db :window-tree new-tree))
       (do
         (println "⚠ No mark set (use C-SPC to set mark first)")
         db)))))

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
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons copied-text kill-ring))]
             (println "✓ Copied" length "characters to kill ring")
             {:db (assoc db :kill-ring updated-kill-ring)
              :fx [[:dispatch [:deactivate-mark]]]})
           {:db db}))
       (do
         (println "⚠ No region selected (set mark with C-SPC first)")
         {:db db})))))

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
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))]
             {:db (assoc db :kill-ring updated-kill-ring)
              :fx [[:dispatch [:deactivate-mark]]
                   [:dispatch [:editor/queue-transaction
                              {:op :delete-range :start start :length length}]]]})
           {:db db}))
       (do
         (println "Mark not set")
         {:db db})))))

;; -- Kill/Yank Commands --

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

;; -- Undo Commands --

(rf/reg-event-fx
 :undo
 (fn [{:keys [db]} [_]]
   "Undo the last command using advanced undo system (C-/ or C-_)"
   (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         undo-stack       (get-in db [:buffers active-buffer-id :undo-stack] [])]

     (if (seq undo-stack)
       (do
         (println "⏪ Advanced Undo: undoing to boundary")
         ;; Disable undo recording during undo operation
         (undo/with-undo-recording-disabled*
           (fn []
             (undo/undo-to-boundary! active-buffer-id)))
         {:db db
          :fx [[:dispatch [:buffer/set-undo-in-progress active-buffer-id true]]
               [:dispatch-later [{:ms 50 :dispatch [:undo-complete active-buffer-id]}]]]})
       (do
         (println "⚠ Nothing to undo")
         {:db db})))))

(rf/reg-event-fx
 :redo
 (fn [{:keys [db]} [_]]
   "Redo the last undone command using advanced undo system (C-g C-_)"
   (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         redo-stack       (get-in db [:buffers active-buffer-id :redo-stack] [])]

     (if (seq redo-stack)
       (do
         (println "⏩ Advanced Redo: redoing last undone command")
         ;; Disable undo recording during redo operation
         (undo/with-undo-recording-disabled*
           (fn []
             (undo/redo! active-buffer-id)))
         {:db db
          :fx [[:dispatch [:buffer/set-undo-in-progress active-buffer-id true]]
               [:dispatch-later [{:ms 50 :dispatch [:undo-complete active-buffer-id]}]]]})
       (do
         (println "⚠ Nothing to redo")
         {:db db})))))

(rf/reg-event-fx
 :undo-complete
 (fn [{:keys [db]} [_ buffer-id]]
   "Reset undo-in-progress flag after undo/redo operation completes"
   {:db db
    :fx [[:dispatch [:buffer/set-undo-in-progress buffer-id false]]]}))

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

(rf/reg-event-db
 :set-cursor-position
 (fn [db [_ position]]
   "Update cursor position"
   (assoc-in db [:ui :cursor-position] position)))

(rf/reg-event-db
 :cursor/set-position
 (fn [db [_ position]]
   "Set cursor position (canonical API for state ownership).

   This is the ONLY event that should update :ui :cursor-position.
   All other modules must dispatch this event instead of direct manipulation.

   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (assoc-in db [:ui :cursor-position] position)))

;; Initialize cursor position when buffer is created
(rf/reg-event-fx
 :initialize-buffer-cursor
 (fn [{:keys [db]} [_ buffer-id]]
   "Initialize cursor position for a buffer"
   (let [^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])]
     (if wasm-instance
       {:fx [[:dispatch [:update-cursor-position 0]]]}
       {:db db}))))

;; -- Keyboard Quit --

(rf/reg-event-fx
 :keyboard-quit
 (fn [{:keys [db]} [_]]
   "Cancel the current operation (equivalent to C-g in Emacs).

   - If minibuffer is active, cancels it and dispatches on-cancel callback
   - Otherwise clears pending operations and shows 'Quit' message

   Issue #67: Must show 'Quit' message in echo area"
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

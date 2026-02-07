(ns lexicon.core.events.edit
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
            [re-frame.db]
            [lexicon.core.db :as db]
            [lexicon.core.constants :as const]
            [lexicon.core.events.buffer :as buffer-events]
            [lexicon.core.api.message]
            [lexicon.core.log :as log]
            [lexicon.core.advanced-undo :as undo]))

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
   "Delete character before cursor (backspace) - queue operation.
    If region is active, deletes the entire region (delete-selection-mode)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         mark-pos (:mark-position active-window)
         current-pos (get-in db [:ui :cursor-position] 0)]
     ;; If region is active (mark is set and different from point), delete region
     (if (and mark-pos (not= mark-pos current-pos))
       (let [start (min mark-pos current-pos)
             end (max mark-pos current-pos)
             length (- end start)]
         {:fx [[:dispatch [:editor/queue-transaction {:op :delete-range :start start :length length}]]
               [:dispatch [:deactivate-mark]]]})
       ;; Otherwise, delete single character
       {:fx [[:dispatch [:editor/queue-transaction {:op :delete-backward}]]]}))))

(rf/reg-event-fx
 :delete-forward-char
 (fn [{:keys [db]} [_]]
   "Delete character after cursor (delete) - queue operation.
    If region is active, deletes the entire region (delete-selection-mode)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         mark-pos (:mark-position active-window)
         current-pos (get-in db [:ui :cursor-position] 0)]
     ;; If region is active, delete region
     (if (and mark-pos (not= mark-pos current-pos))
       (let [start (min mark-pos current-pos)
             end (max mark-pos current-pos)
             length (- end start)]
         {:fx [[:dispatch [:editor/queue-transaction {:op :delete-range :start start :length length}]]
               [:dispatch [:deactivate-mark]]]})
       ;; Otherwise, delete single character
       {:fx [[:dispatch [:editor/queue-transaction {:op :delete-forward}]]]}))))

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
 (fn [{:keys [db]} [_ & args]]
   "Move cursor backward N characters (C-b or Left arrow)
    With prefix arg, moves that many characters."
   (let [current-pos (get-in db [:ui :cursor-position] 0)
         ;; Use prefix-arg if available (Phase 6.5)
         prefix-arg (:current-prefix-arg db)
         n (if prefix-arg
             (cond
               (number? prefix-arg) prefix-arg
               (list? prefix-arg) (first prefix-arg)
               :else 1)
             1)
         new-pos (max 0 (- current-pos n))]
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

;; -- Shift+Arrow Selection Commands --
;; These commands extend the region (selection) by setting mark if not set, then moving point.

(rf/reg-event-fx
 :forward-char-shift
 (fn [{:keys [db]} [_]]
   "Move forward one character while extending selection (Shift+Right)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         mark-pos (:mark-position active-window)
         current-pos (get-in db [:ui :cursor-position] 0)]
     ;; If mark is not set, set it first at current position
     (if mark-pos
       {:fx [[:dispatch [:forward-char]]]}
       {:fx [[:dispatch [:set-mark]]
             [:dispatch [:forward-char]]]}))))

(rf/reg-event-fx
 :backward-char-shift
 (fn [{:keys [db]} [_]]
   "Move backward one character while extending selection (Shift+Left)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         mark-pos (:mark-position active-window)]
     (if mark-pos
       {:fx [[:dispatch [:backward-char]]]}
       {:fx [[:dispatch [:set-mark]]
             [:dispatch [:backward-char]]]}))))

(rf/reg-event-fx
 :next-line-shift
 (fn [{:keys [db]} [_]]
   "Move to next line while extending selection (Shift+Down)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         mark-pos (:mark-position active-window)]
     (if mark-pos
       {:fx [[:dispatch [:next-line]]]}
       {:fx [[:dispatch [:set-mark]]
             [:dispatch [:next-line]]]}))))

(rf/reg-event-fx
 :previous-line-shift
 (fn [{:keys [db]} [_]]
   "Move to previous line while extending selection (Shift+Up)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         mark-pos (:mark-position active-window)]
     (if mark-pos
       {:fx [[:dispatch [:previous-line]]]}
       {:fx [[:dispatch [:set-mark]]
             [:dispatch [:previous-line]]]}))))

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

(defn kill-command?
  "Check if a command is a kill command that should append to kill ring"
  [cmd]
  (contains? #{:kill-line :kill-word :backward-kill-word :kill-region :edit/kill-region} cmd))

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
                 last-command (:last-command db)
                 ;; Consecutive kill commands should append to the previous kill
                 should-append? (and (seq kill-ring) (kill-command? last-command))
                 updated-kill-ring (if should-append?
                                    (cons (str (first kill-ring) killed-text)
                                          (rest kill-ring))
                                    (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring)))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc :last-command :kill-word))
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
                 last-command (:last-command db)
                 ;; Consecutive kill commands should append to the previous kill
                 ;; Note: For backward kills, we prepend instead of append
                 should-append? (and (seq kill-ring) (kill-command? last-command))
                 updated-kill-ring (if should-append?
                                    (cons (str killed-text (first kill-ring))
                                          (rest kill-ring))
                                    (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring)))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc :last-command :backward-kill-word))
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
                 last-command (:last-command db)
                 ;; Consecutive kill commands should append to the previous kill
                 should-append? (and (seq kill-ring) (kill-command? last-command))
                 updated-kill-ring (if should-append?
                                    ;; Append to most recent kill entry
                                    (cons (str (first kill-ring) killed-text)
                                          (rest kill-ring))
                                    ;; Push new kill entry
                                    (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring)))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc :last-command :kill-line))
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
                 last-command (:last-command db)
                 ;; Consecutive kill commands should append to the previous kill
                 should-append? (and (seq kill-ring) (kill-command? last-command))
                 updated-kill-ring (if should-append?
                                    (cons (str (first kill-ring) killed-text)
                                          (rest kill-ring))
                                    (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring)))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc :last-command :kill-region))
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
             yank-length (count text-to-yank)
             ;; Use explicit position to ensure yank-pop has correct bounds
             insert-pos (or cursor-pos 0)]
         (println "✓ Yanking" yank-length "chars at position" insert-pos)
         {:db (-> db
                  (assoc :last-yank {:position insert-pos
                                     :length yank-length
                                     :kill-ring-index 0})
                  (assoc :last-command :yank))
          :fx [[:dispatch [:editor/queue-transaction
                           {:op :insert :text text-to-yank :position insert-pos}]]]})
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
   "Undo the last command using advanced undo system (C-/ or C-_)

   Emacs-style redo: After C-g breaks the undo chain, the next C-/ does redo."
   (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         buffer-name      (get-in db [:buffers active-buffer-id :name])
         undo-stack       (get-in db [:buffers active-buffer-id :undo-stack] [])
         redo-stack       (get-in db [:buffers active-buffer-id :redo-stack] [])
         undo-chain-broken? (:undo-chain-broken db)
         stack-size       (count undo-stack)]

     ;; Emacs-style redo: if chain was broken by C-g and redo-stack has content,
     ;; do redo instead of undo
     (if (and undo-chain-broken? (seq redo-stack))
       (do
         ;; Clear the chain-broken flag and do redo
         (undo/with-undo-recording-disabled*
           (fn []
             (undo/redo! active-buffer-id)))
         {:db (dissoc db :undo-chain-broken)
          :fx [[:dispatch [:buffer/set-undo-in-progress active-buffer-id true]]
               [:dispatch-later [{:ms 50 :dispatch [:undo-complete active-buffer-id]}]]]})
       ;; Normal undo
       (if (seq undo-stack)
         (do
           ;; Disable undo recording during undo operation
           ;; Clear chain-broken flag on any undo
           (undo/with-undo-recording-disabled*
             (fn []
               (undo/undo-to-boundary! active-buffer-id)))
           {:db (dissoc db :undo-chain-broken)
            :fx [[:dispatch [:buffer/set-undo-in-progress active-buffer-id true]]
                 [:dispatch-later [{:ms 50 :dispatch [:undo-complete active-buffer-id]}]]]})
         {:db (dissoc db :undo-chain-broken)})))))

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
   "Reset undo-in-progress flag after undo/redo operation completes and trigger view update"
   (let [^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
         text (when wasm-instance (.getText wasm-instance))
         lines (when text (clojure.string/split text #"\n" -1))
         line-count (if lines (count lines) 1)
         ;; Get point position from buffer (set by undo operation)
         point-pos (get-in db [:buffers buffer-id :point] 0)
         line-col (when text (linear-pos-to-line-col text point-pos))
         active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         new-window-tree (db/update-window-in-tree window-tree active-window-id
                                                   #(assoc % :cursor-position (or line-col {:line 0 :column 0})))]
     {:db (-> db
              (assoc :window-tree new-window-tree)
              (assoc-in [:buffers buffer-id :cache :text] (or text ""))
              (assoc-in [:buffers buffer-id :cache :line-count] line-count)
              (assoc-in [:ui :cursor-position] point-pos)
              (assoc-in [:ui :view-needs-update?] true))
      :fx [[:dispatch [:buffer/set-undo-in-progress buffer-id false]]
           [:dispatch [:buffer/increment-version buffer-id]]
           [:dispatch [:cursor/set-position point-pos]]]})))

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
                  (assoc-in [:buffers active-buffer-id :cursor-position] line-col)  ; Save line/col to buffer
                  (assoc-in [:buffers active-buffer-id :point] clamped-pos)  ; Save linear position to buffer
                  (assoc-in [:ui :cursor-position] clamped-pos))})
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
   - Sets :undo-chain-broken flag for Emacs-style redo
   - Clears pending operations and shows 'Quit' message

   Issue #67: Must show 'Quit' message in echo area
   Issue #150: Breaks undo chain for Emacs-style redo (C-g C-/ = redo)"
   (let [minibuffer-active? (get-in db [:minibuffer :active?])
         on-cancel (get-in db [:minibuffer :on-cancel] [:minibuffer/deactivate])
         ;; Mark that undo chain was broken - next undo should redo if available
         db-with-chain-broken (assoc db :undo-chain-broken true)]
     (if minibuffer-active?
       ;; If minibuffer is active, cancel it and dispatch on-cancel
       {:db (-> db-with-chain-broken
                (assoc-in [:ui :selection] {:start 0 :end 0})
                (assoc-in [:fsm :operator-pending] nil))
        :fx [[:dispatch on-cancel]
             [:dispatch [:echo/message "Quit"]]]}
       ;; Otherwise just clear pending operations and show quit message
       {:db (-> db-with-chain-broken
                (assoc-in [:ui :selection] {:start 0 :end 0})
                (assoc-in [:fsm :operator-pending] nil))
        :fx [[:dispatch [:echo/message "Quit"]]]}))))

;; -- Test API Events --
;; Simple buffer-specific versions for testing

(rf/reg-event-fx
 :edit/kill-region
 (fn [{:keys [db]} [_ buffer-id start end]]
   "Kill (cut) text from start to end position in buffer (test API).

   Args:
     buffer-id - Buffer to kill from
     start - Start position
     end - End position"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         buffer-text-before (when wasm-instance (.getText ^js wasm-instance))
         text-to-kill (when buffer-text-before
                        (subs buffer-text-before start end))
         kill-ring (get db :kill-ring [])
         last-command (:last-command db)

         ;; Check if we should append to last kill (consecutive kills)
         should-append? (= last-command :edit/kill-region)

         new-kill-ring (if should-append?
                        ;; Append to most recent kill
                        (update kill-ring (dec (count kill-ring))
                                (fn [prev-kill] (str prev-kill text-to-kill)))
                        ;; Push new kill
                        (conj kill-ring text-to-kill))]

     (when wasm-instance
       (.delete ^js wasm-instance start (- end start)))

     (let [buffer-text-after (when wasm-instance (.getText ^js wasm-instance))
           log-msg (str "KILL[" buffer-id "]: before=" (pr-str buffer-text-before)
                       " killed=" (pr-str text-to-kill)
                       " after=" (pr-str buffer-text-after)
                       " should-append=" should-append?
                       " last-cmd=" last-command
                       " kill-ring=" (pr-str new-kill-ring))]


       (let [recording-enabled? (get-in db [:undo :recording-enabled?] true)
             new-db (-> db
                        (assoc :kill-ring new-kill-ring)
                        (assoc :last-command :edit/kill-region)
                        ;; Set point to start of killed region (Emacs behavior)
                        (assoc-in [:buffers buffer-id :point] start))
             ;; Record undo entry directly (synchronous)
             final-db (if (and wasm-instance recording-enabled?)
                        (let [undo-entry {:type :edit
                                          :op :delete-range
                                          :start start
                                          :length (- end start)
                                          :text text-to-kill}
                              stack (get-in new-db [:buffers buffer-id :undo-stack] [])
                              last-entry (peek stack)
                              should-add-boundary? (and (seq stack)
                                                        (not= (:type last-entry) :boundary))
                              updated-stack (cond-> (conj stack undo-entry)
                                              should-add-boundary?
                                              (conj {:type :boundary}))]
                          (assoc-in new-db [:buffers buffer-id :undo-stack] updated-stack))
                        new-db)]
         {:db final-db})))))

(rf/reg-event-fx
 :edit/yank
 (fn [{:keys [db]} [_ buffer-id position]]
   "Yank (paste) from kill ring at position in buffer (test API).

   Args:
     buffer-id - Buffer to yank into
     position - Position to insert at"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         buffer-text-before (when wasm-instance (.getText ^js wasm-instance))
         kill-ring (get db :kill-ring [])
         text-to-yank (peek kill-ring)]

     (when (and wasm-instance text-to-yank)
       (.insert ^js wasm-instance position text-to-yank))

     (let [buffer-text-after (when wasm-instance (.getText ^js wasm-instance))
           log-msg (str "YANK[" buffer-id "]: before=" (pr-str buffer-text-before)
                       " yanked=" (pr-str text-to-yank)
                       " pos=" position
                       " after=" (pr-str buffer-text-after)
                       " kill-ring=" (pr-str kill-ring))]


       (let [recording-enabled? (get-in db [:undo :recording-enabled?] true)
             new-db (assoc db :last-command :yank)
             ;; Record undo entry directly (synchronous)
             final-db (if (and wasm-instance text-to-yank recording-enabled?)
                        (let [undo-entry {:type :edit
                                          :op :insert
                                          :position position
                                          :text text-to-yank}
                              stack (get-in new-db [:buffers buffer-id :undo-stack] [])
                              last-entry (peek stack)
                              should-add-boundary? (and (seq stack)
                                                        (not= (:type last-entry) :boundary))
                              updated-stack (cond-> (conj stack undo-entry)
                                              should-add-boundary?
                                              (conj {:type :boundary}))]
                          (assoc-in new-db [:buffers buffer-id :undo-stack] updated-stack))
                        new-db)]
         {:db final-db})))))

(rf/reg-event-fx
 :edit/undo
 (fn [{:keys [db]} [_ buffer-id]]
   "Undo last change in buffer (test API).

   Args:
     buffer-id - Buffer to undo in"
   ;; Use the advanced undo system
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         buffer-text-before (when wasm-instance (.getText ^js wasm-instance))
         undo-stack (get-in db [:buffers buffer-id :undo-stack] [])]
     (if (seq undo-stack)
       ;; Perform undo directly in same event (synchronous for tests)
       (let [{:keys [entries boundary-index]} (undo/extract-undo-group undo-stack (count undo-stack))
             ;; Filter edit and marker entries
             undoable-entries (filter #(#{:edit :marker} (:type %)) entries)
             ;; Reverse order for undo
             reversed-entries (reverse undoable-entries)
             ;; Find point marker to restore
             marker-entry (first (filter #(and (= (:type %) :marker)
                                                (= (:marker-id %) :point))
                                          reversed-entries))]

         ;; Apply undo entries
         (doseq [entry reversed-entries]
           (let [inverted (undo/invert-undo-entry entry)]
             (case (:type entry)
               :edit (undo/apply-undo-entry buffer inverted)
               :marker nil
               nil)))

         ;; Update db with new undo/redo stacks AND restore point
         ;; Also sync cursor position directly (no async dispatch)
         (let [point-pos (when marker-entry (:old-pos marker-entry))
               text (when wasm-instance (.getText ^js wasm-instance))
               line-col (when (and text point-pos) (linear-pos-to-line-col text point-pos))
               active-window-id (:active-window-id db)
               window-tree (:window-tree db)
               new-window-tree (if line-col
                                 (db/update-window-in-tree window-tree active-window-id
                                                           #(assoc % :cursor-position line-col))
                                 window-tree)
               new-db (-> db
                          (assoc-in [:buffers buffer-id :undo-stack]
                                    (subvec undo-stack 0 boundary-index))
                          (update-in [:buffers buffer-id :redo-stack]
                                     (fn [redo]
                                       (conj (or redo []) {:entries entries :boundary-index boundary-index})))
                          ;; Restore point from marker if present
                          (cond-> marker-entry
                            (assoc-in [:buffers buffer-id :point] (:old-pos marker-entry)))
                          ;; Sync cursor position to window tree
                          (assoc :window-tree new-window-tree)
                          ;; Update UI cursor position
                          (cond-> point-pos
                            (assoc-in [:ui :cursor-position] point-pos)))]
           {:db new-db}))
       {:db db}))))

(rf/reg-event-fx
 :log-undo-result
 (fn [{:keys [db]} [_ buffer-id buffer-text-before]]
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         buffer-text-after (when wasm-instance (.getText ^js wasm-instance))
         undo-stack (get-in db [:buffers buffer-id :undo-stack] [])
         log-msg (str "UNDO[" buffer-id "]: before=" (pr-str buffer-text-before)
                     " after=" (pr-str buffer-text-after)
                     " stack-size=" (count undo-stack))]
     (js/console.log log-msg)
     {:db db})))

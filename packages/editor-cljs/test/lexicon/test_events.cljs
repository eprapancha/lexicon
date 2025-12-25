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
       (.delete ^js wasm start end))
     (update-in db [:buffers buffer-id :editor-version] (fnil inc 0)))))

(rf/reg-event-db
 :set-current-buffer
 (fn [db [_ buffer-id]]
   "Set the current buffer to BUFFER-ID (for testing)"
   (if (get-in db [:buffers buffer-id])
     (let [active-window-id (get-in db [:active-window-id])]
       (-> db
           (assoc-in [:editor :current-buffer-id] buffer-id)
           (assoc-in [:windows active-window-id :buffer-id] buffer-id)))
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
         ;; Update cursor position and version
         (-> db
             (assoc-in [:ui :cursor-position] (inc point))
             (update-in [:buffers buffer-id :editor-version] (fnil inc 0))))
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
         ;; Update cursor position and version
         (-> db
             (assoc-in [:ui :cursor-position] (inc point))
             (update-in [:buffers buffer-id :editor-version] (fnil inc 0))))
       db))))

;; Cursor movement events (synchronous for testing)
(rf/reg-event-db
 :forward-char
 (fn [db [_]]
   "Move cursor forward one character (for testing)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
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
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
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
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
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

(rf/reg-event-fx
 :split-window-horizontally
 (fn [{:keys [db]} [_]]
   "Split current window horizontally (for testing)"
   {:fx [[:dispatch [:split-window-right]]]}))

(rf/reg-event-fx
 :split-window-vertically
 (fn [{:keys [db]} [_]]
   "Split current window vertically (for testing)"
   {:fx [[:dispatch [:split-window-below]]]}))

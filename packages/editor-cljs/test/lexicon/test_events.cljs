(ns lexicon.test-events
  "Test-specific event handlers for testing Lexicon editor.

  These events are simplified versions that bypass normal editor workflows
  to allow direct manipulation of buffers for testing purposes."
  (:require [re-frame.core :as rf]))

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
     (let [active-window-id (get-in db [:editor :active-window-id])]
       (-> db
           (assoc-in [:editor :current-buffer-id] buffer-id)
           (assoc-in [:windows active-window-id :buffer-id] buffer-id)))
     db)))

(rf/reg-event-fx
 :self-insert-command
 (fn [{:keys [db]} [_ char]]
   "Insert CHAR at point in current buffer (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)]  ; Get from db, not WASM
     (if wasm
       {:db (-> db
                (update-in [:buffers buffer-id :editor-version] (fnil inc 0)))
        :fx [[:dispatch [:buffer/insert buffer-id point char]]]}
       {:db db}))))

(rf/reg-event-fx
 :newline
 (fn [{:keys [db]} [_]]
   "Insert newline at point in current buffer (for testing)"
   (let [buffer-id (get-in db [:editor :current-buffer-id])
         buffer (get-in db [:buffers buffer-id])
         ^js wasm (:wasm-instance buffer)
         point (get-in db [:ui :cursor-position] 0)]  ; Get from db, not WASM
     (if wasm
       {:fx [[:dispatch [:buffer/insert buffer-id point "\n"]]]}
       {:db db}))))

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

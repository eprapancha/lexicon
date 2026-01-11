(ns lexicon.events.message
  "Event handlers for message display and *Messages* buffer (Issue #47).

  Handles:
  - Appending messages to *Messages* buffer
  - Flashing messages in minibuffer
  - Auto-clearing minibuffer messages after timeout"
  (:require [re-frame.core :as rf]
            [re-frame.db]))

(defn- get-timestamp
  "Get current timestamp in Emacs format: HH:MM:SS"
  []
  (let [now (js/Date.)
        hours (.getHours now)
        minutes (.getMinutes now)
        seconds (.getSeconds now)
        pad (fn [n] (if (< n 10) (str "0" n) (str n)))]
    (str (pad hours) ":" (pad minutes) ":" (pad seconds))))

(defn- append-to-messages-buffer!
  "Append a message with timestamp to *Messages* buffer.

  Args:
    db - App database
    msg - Message string

  Returns:
    Updated database"
  [db msg]
  (let [messages-buffer (get-in db [:buffers 2])
        ^js wasm-instance (:wasm-instance messages-buffer)]
    (when wasm-instance
      (let [timestamp (get-timestamp)
            current-text (.getText wasm-instance)
            new-line (str "[" timestamp "] " msg "\n")
            buffer-length (.length wasm-instance)]
        ;; Append message to end of buffer
        (.insert wasm-instance buffer-length new-line)
        ;; Update cache
        (let [updated-text (.getText wasm-instance)
              updated-line-count (count (clojure.string/split updated-text #"\n" -1))]
          (-> db
              (assoc-in [:buffers 2 :cache :text] updated-text)
              (assoc-in [:buffers 2 :cache :line-count] updated-line-count)
              (update-in [:buffers 2 :editor-version] inc)))))))

(rf/reg-event-fx
 :message/display
 (fn [{:keys [db]} [_ msg]]
   "Display a message: append to *Messages* buffer and flash in minibuffer.

   Issue #47: Unified message system"
   (let [;; Append to *Messages* buffer
         updated-db (append-to-messages-buffer! db msg)
         ;; Set up minibuffer flash with auto-clear timeout
         timeout-id (js/setTimeout
                     #(rf/dispatch [:message/clear-minibuffer])
                     2000)]  ; 2 second timeout
     {:db (-> updated-db
              (assoc-in [:minibuffer :message] msg)
              (assoc-in [:minibuffer :message-timeout-id] timeout-id))})))

(rf/reg-event-db
 :message/clear-minibuffer
 (fn [db [_]]
   "Clear the message from minibuffer after timeout"
   (let [timeout-id (get-in db [:minibuffer :message-timeout-id])]
     (when timeout-id
       (js/clearTimeout timeout-id))
     (-> db
         (assoc-in [:minibuffer :message] "")
         (assoc-in [:minibuffer :message-timeout-id] nil)))))

(comment
  ;; Usage:
  (rf/dispatch [:message/display "Hello, world!"])
  (rf/dispatch [:message/display "File saved successfully"]))

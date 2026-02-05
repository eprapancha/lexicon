(ns lexicon.core.packages.autorevert
  "Auto-revert mode - automatically revert buffers when files change on disk.

  Implements Emacs autorevert.el functionality:
  - auto-revert-mode: Per-buffer auto-refresh
  - global-auto-revert-mode: Enable for all file buffers
  - Polling timer to check for file modifications
  - Reverts buffer content when external changes detected

  In the browser, we use the File System Access API to re-read files
  and compare content/modification time.

  Based on Emacs lisp/autorevert.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def auto-revert-interval
  "Interval in milliseconds between checks for file modifications.
   Default is 5000ms (5 seconds), matching Emacs default."
  5000)

;; =============================================================================
;; Timer Management
;; =============================================================================

(defonce auto-revert-timer (atom nil))

(defn- cancel-timer!
  "Cancel the auto-revert polling timer."
  []
  (when-let [t @auto-revert-timer]
    (js/clearInterval t)
    (reset! auto-revert-timer nil)))

(defn- start-timer!
  "Start the auto-revert polling timer."
  []
  (cancel-timer!)
  (reset! auto-revert-timer
          (js/setInterval
           (fn [] (rf/dispatch [:auto-revert/check-buffers]))
           auto-revert-interval)))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :auto-revert/check-buffers
 (fn [{:keys [db]} [_]]
   "Check all auto-revert enabled buffers for changes."
   (let [global? (get-in db [:settings :global-auto-revert-mode] false)
         buffers (:buffers db)
         file-buffers (filter (fn [[_id buf]]
                                (and (:file-handle buf)
                                     (or global?
                                         (get-in buf [:minor-modes :auto-revert-mode]))))
                              buffers)]
     (when (seq file-buffers)
       {:fx (mapv (fn [[buffer-id _buf]]
                    [:dispatch [:auto-revert/check-single-buffer buffer-id]])
                  file-buffers)}))))

(rf/reg-event-fx
 :auto-revert/check-single-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Check a single buffer for external modifications.
    Uses the File System Access API to re-read the file."
   (let [buffer (get-in db [:buffers buffer-id])
         file-handle (:file-handle buffer)]
     (when (and file-handle (not (:is-modified? buffer)))
       ;; Only check unmodified buffers - don't overwrite user changes
       {:auto-revert/read-file {:buffer-id buffer-id
                                :file-handle file-handle}}))))

(rf/reg-fx
 :auto-revert/read-file
 (fn [{:keys [buffer-id file-handle]}]
   "Read file via File System Access API and compare with buffer content."
   (when file-handle
     (-> (.getFile file-handle)
         (.then (fn [file]
                  (-> (.text file)
                      (.then (fn [content]
                               (rf/dispatch [:auto-revert/compare-content
                                             buffer-id content]))))))
         (.catch (fn [_err]
                   ;; File may have been deleted or permission lost - ignore
                   nil))))))

(rf/reg-event-fx
 :auto-revert/compare-content
 (fn [{:keys [db]} [_ buffer-id new-content]]
   "Compare file content with buffer content and revert if different."
   (let [buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         current-text (when wasm (try (.getText wasm) (catch :default _ nil)))]
     (when (and current-text (not= current-text new-content))
       ;; Content differs - revert
       {:fx [[:dispatch [:auto-revert/revert-buffer buffer-id new-content]]]}))))

(rf/reg-event-fx
 :auto-revert/revert-buffer
 (fn [{:keys [db]} [_ buffer-id new-content]]
   "Revert buffer to new content."
   (let [buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         verbose? (get-in db [:settings :auto-revert-verbose] true)]
     (when wasm
       ;; Replace buffer content via WASM gap buffer
       (let [^js w wasm
             current-len (.-length w)]
         (.delete w 0 current-len)
         (.insert w 0 new-content))
       (let [lines (str/split new-content #"\n" -1)
             line-count (count lines)]
         (merge
          {:db (-> db
                   (assoc-in [:buffers buffer-id :is-modified?] false)
                   (assoc-in [:buffers buffer-id :cache :text] new-content)
                   (assoc-in [:buffers buffer-id :cache :line-count] line-count))}
          (when verbose?
            {:fx [[:dispatch [:echo/message
                              (str "Reverted buffer " (:name buffer))]]]})))))))

(rf/reg-event-db
 :auto-revert/toggle-mode
 (fn [db [_ buffer-id]]
   "Toggle auto-revert-mode for a buffer."
   (let [buffer-id (or buffer-id
                       (let [window (get-in db [:window-tree])]
                         (when (= (:type window) :leaf)
                           (:buffer-id window))))]
     (if buffer-id
       (update-in db [:buffers buffer-id :minor-modes]
                  (fn [modes]
                    (let [modes (or modes #{})]
                      (if (contains? modes :auto-revert-mode)
                        (disj modes :auto-revert-mode)
                        (conj modes :auto-revert-mode)))))
       db))))

(rf/reg-event-fx
 :auto-revert/toggle-global-mode
 (fn [{:keys [db]} [_]]
   "Toggle global-auto-revert-mode."
   (let [currently-enabled? (get-in db [:settings :global-auto-revert-mode] false)
         new-state (not currently-enabled?)]
     (if new-state
       (start-timer!)
       (cancel-timer!))
     {:db (assoc-in db [:settings :global-auto-revert-mode] new-state)
      :fx [[:dispatch [:echo/message (if new-state
                                       "global-auto-revert-mode enabled"
                                       "global-auto-revert-mode disabled")]]]})))

;; =============================================================================
;; Commands
;; =============================================================================

(rf/reg-event-fx
 :command/auto-revert-mode
 (fn [{:keys [_db]} [_]]
   {:fx [[:dispatch [:auto-revert/toggle-mode nil]]
         [:dispatch [:echo/message "auto-revert-mode toggled"]]]}))

(rf/reg-event-fx
 :command/global-auto-revert-mode
 (fn [{:keys [_db]} [_]]
   {:fx [[:dispatch [:auto-revert/toggle-global-mode]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize autorevert module and register commands."
  []
  (rf/dispatch [:register-command :auto-revert-mode
                {:docstring "Toggle auto-revert mode - revert buffer when file changes on disk"
                 :interactive nil
                 :handler [:command/auto-revert-mode]}])

  (rf/dispatch [:register-command :global-auto-revert-mode
                {:docstring "Toggle global auto-revert mode for all file buffers"
                 :interactive nil
                 :handler [:command/global-auto-revert-mode]}]))

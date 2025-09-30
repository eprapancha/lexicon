(ns lexicon.subs
  (:require [re-frame.core :as rf]
            [lexicon.cache :as cache]
            [lexicon.wasm-utils :as wasm]))

;; -- Base Subscriptions --

(rf/reg-sub
 :initialized?
 (fn [db _]
   "Check if the application is fully initialized"
   (:initialized? db)))

(rf/reg-sub
 :active-window-id
 (fn [db _]
   "Get the currently active window ID"
   (:active-window-id db)))

(rf/reg-sub
 :windows
 (fn [db _]
   "Get all windows"
   (:windows db)))

(rf/reg-sub
 :active-window
 :<- [:windows]
 :<- [:active-window-id]
 (fn [[windows active-id] _]
   "Get the currently active window"
   (get windows active-id)))

;; -- Buffer Subscriptions --

(rf/reg-sub
 :buffers
 (fn [db _]
   "Get all buffers"
   (:buffers db)))

(rf/reg-sub
 :active-buffer
 :<- [:buffers]
 :<- [:active-window]
 (fn [[buffers active-window] _]
   "Get the currently active buffer"
   (when active-window
     (get buffers (:buffer-id active-window)))))

(rf/reg-sub
 :active-wasm-instance
 :<- [:active-buffer]
 (fn [active-buffer _]
   "Get the WASM instance for the active buffer"
   (:wasm-instance active-buffer)))

(rf/reg-sub
 :buffer-content
 :<- [:active-wasm-instance]
 :<- [:view-needs-update?]
 :<- [:last-transaction-id]
 (fn [[^js wasm-instance view-needs-update? transaction-id] _]
   "Get the content of the active buffer from WASM"
   (if wasm-instance
     (.getText wasm-instance)
     "")))

(rf/reg-sub
 :visible-content
 :<- [:active-wasm-instance]
 :<- [:ui]
 (fn [[^js wasm-instance ui] [_ start end]]
   "Get visible text range with caching"
   (if wasm-instance
     (let [cache (:text-cache ui)
           actual-start (or start (get-in ui [:viewport :start]) 0)
           actual-end (or end (get-in ui [:viewport :end]) 1000)
           [content _] (cache/get-text-range cache wasm-instance actual-start actual-end)]
       content)
     "")))

(rf/reg-sub
 :text-range
 :<- [:active-wasm-instance]
 (fn [^js wasm-instance [_ start end]]
   "Get specific text range without caching (for small ranges)"
   (if wasm-instance
     (first (wasm/get-text-range-safe wasm-instance start end))
     "")))

(rf/reg-sub
 :viewport
 (fn [db _]
   "Get current viewport range"
   (get-in db [:ui :viewport])))

(rf/reg-sub
 :text-cache-stats
 (fn [db _]
   "Get text cache statistics for debugging"
   (cache/cache-stats (get-in db [:ui :text-cache]))))

(rf/reg-sub
 :buffer-length
 :<- [:active-wasm-instance]
 (fn [^js wasm-instance _]
   "Get the length of the active buffer from WASM"
   (if wasm-instance
     (.getLength wasm-instance)
     0)))

;; -- UI State Subscriptions --

(rf/reg-sub
 :cursor-position
 (fn [db _]
   "Get current cursor position"
   (get-in db [:ui :cursor-position])))

(rf/reg-sub
 :selection
 (fn [db _]
   "Get current selection range"
   (get-in db [:ui :selection])))

(rf/reg-sub
 :ime-composing?
 (fn [db _]
   "Check if IME is currently composing"
   (get-in db [:ui :ime-composing?])))

(rf/reg-sub
 :ime-composition-text
 (fn [db _]
   "Get current IME composition text"
   (get-in db [:ui :ime-composition-text])))

(rf/reg-sub
 :view-needs-update?
 (fn [db _]
   "Check if view needs to be updated"
   (get-in db [:ui :view-needs-update?])))

;; -- Editor State Subscriptions --

(rf/reg-sub
 :editor-mode
 (fn [db _]
   "Get current editor mode"
   (get-in db [:editor :mode])))

(rf/reg-sub
 :active-keymap
 (fn [db _]
   "Get current active keymap"
   (get-in db [:editor :keymap])))

;; -- System State Subscriptions --

(rf/reg-sub
 :last-transaction-id
 (fn [db _]
   "Get the last transaction ID"
   (get-in db [:system :last-transaction-id])))

(rf/reg-sub
 :mutation-observer
 (fn [db _]
   "Get the MutationObserver instance"
   (get-in db [:system :mutation-observer])))

(rf/reg-sub
 :reconciliation-active?
 (fn [db _]
   "Check if reconciliation is currently active"
   (get-in db [:system :reconciliation-active?])))

;; -- Derived Subscriptions --

(rf/reg-sub
 :buffer-modified?
 :<- [:active-buffer]
 (fn [buffer _]
   "Check if the active buffer has been modified"
   (:is-modified? buffer)))

(rf/reg-sub
 :kill-ring
 (fn [db _]
   "Get the kill ring"
   (:kill-ring db)))

(rf/reg-sub
 :selection-active?
 :<- [:selection]
 (fn [selection _]
   "Check if there is an active selection"
   (not= (:start selection) (:end selection))))

(rf/reg-sub
 :editor-ready?
 :<- [:initialized?]
 :<- [:active-buffer]
 (fn [[initialized? buffer] _]
   "Check if the editor is ready for user interaction"
   (and initialized? buffer)))

(rf/reg-sub
 :wasm-error
 (fn [db _]
   "Get WASM loading error if any"
   (get-in db [:system :wasm-error])))

;; -- Virtualized Rendering Subscriptions --

(rf/reg-sub
 ::viewport
 :<- [:active-window]
 (fn [active-window _]
   "Get the viewport for the active window"
   (:viewport active-window)))

(rf/reg-sub
 ::total-lines
 :<- [:active-wasm-instance]
 (fn [^js wasm-instance _]
   "Get the total number of lines in the document"
   (if wasm-instance
     (.lineCount wasm-instance)
     1)))

(rf/reg-sub
 ::visible-lines
 :<- [::viewport]
 :<- [:active-wasm-instance]
 (fn [[viewport ^js wasm-instance] _]
   "Get the text for the visible lines"
   (if (and wasm-instance viewport)
     (let [{:keys [start-line end-line]} viewport]
       (.getTextForLineRange wasm-instance start-line end-line))
     "")))

(rf/reg-sub
 :line-height
 (fn [db _]
   "Get the line height for layout calculations"
   (:line-height db)))
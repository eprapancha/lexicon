(ns lexicon.core.subs
  (:require [re-frame.core :as rf]
            [lexicon.core.cache :as cache]
            [lexicon.core.wasm-utils :as wasm]
            [lexicon.core.db :as db]
            [lexicon.core.faces :as faces]))

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
 :cursor-owner
 (fn [db _]
   "Get the ID of the window or component that currently owns the cursor (Issue #62)"
   (:cursor-owner db)))

(rf/reg-sub
 :window-tree
 (fn [db _]
   "Get the window tree"
   (:window-tree db)))

(rf/reg-sub
 :active-window
 :<- [:window-tree]
 :<- [:active-window-id]
 (fn [[window-tree active-id] _]
   "Get the currently active window"
   (db/find-window-in-tree window-tree active-id)))

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
 :<- [:active-buffer]
 (fn [active-buffer _]
   "Get the content of the active buffer from cache"
   (get-in active-buffer [:cache :text] "")))

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
     (.length wasm-instance)
     0)))

;; -- Window-Based Cursor Subscriptions (Phase 3) --

(rf/reg-sub
 ::cursor-position
 :<- [:active-window]
 (fn [active-window _]
   "Get the cursor position from the active window"
   (:cursor-position active-window)))

(rf/reg-sub
 ::selection-range
 :<- [:active-buffer]
 (fn [active-buffer _]
   "Get the selection range from the active buffer"
   (:selection-range active-buffer)))

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
 :show-line-numbers?
 (fn [db _]
   "Check if line numbers should be shown in status bar (Phase 5)"
   (get-in db [:ui :show-line-numbers?] true)))

(rf/reg-sub
 :show-column-number?
 (fn [db _]
   "Check if column number should be shown in status bar (Phase 5)"
   (get-in db [:ui :show-column-number?] true)))

(rf/reg-sub
 :view-needs-update?
 (fn [db _]
   "Check if view needs to be updated"
   (get-in db [:ui :view-needs-update?])))

(rf/reg-sub
 :prefix-key-state
 (fn [db _]
   "Get active prefix key state (e.g., 'C-x' when waiting for next key).
   Issue #137: Used by minibuffer to route keys to keybinding system."
   (get-in db [:ui :prefix-key-state])))

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

;; -- FSM State Subscriptions --

(rf/reg-sub
 :fsm/current-state
 (fn [db _]
   "Get the current FSM state"
   (get-in db [:fsm :current-state])))

(rf/reg-sub
 :fsm/previous-state
 (fn [db _]
   "Get the previous FSM state"
   (get-in db [:fsm :previous-state])))

(rf/reg-sub
 :fsm/operator-pending
 (fn [db _]
   "Get the pending operator if any"
   (get-in db [:fsm :operator-pending])))

(rf/reg-sub
 :fsm/active-keymap
 (fn [db _]
   "Get the current active keymap from FSM"
   (get-in db [:fsm :active-keymap])))

(rf/reg-sub
 :fsm
 (fn [db _]
   "Get the entire FSM state"
   (:fsm db)))

(rf/reg-sub
 :modal-status-display
 :<- [:fsm/current-state]
 :<- [:fsm/operator-pending]
 (fn [[current-state operator-pending] _]
   "Get display string for modal status bar"
   (cond
     operator-pending (str "-- " (name current-state) " [" (name operator-pending) "] --")
     :else (str "-- " (name current-state) " --"))))

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
 :<- [:active-buffer]
 (fn [active-buffer _]
   "Get the total number of lines in the document from cache"
   (get-in active-buffer [:cache :line-count] 1)))

(rf/reg-sub
 ::visible-lines
 :<- [::viewport]
 :<- [:active-buffer]
 (fn [[viewport active-buffer] _]
   "Get the text for the visible lines from cache"
   (if (and active-buffer viewport)
     (let [{:keys [start-line end-line]} viewport
           full-text (get-in active-buffer [:cache :text] "")
           all-lines (clojure.string/split full-text #"\n" -1)  ; -1 keeps trailing empty strings
           visible-lines (subvec (vec all-lines) start-line (min (inc end-line) (count all-lines)))
           result (clojure.string/join "\n" visible-lines)]
       result)
     "")))

(rf/reg-sub
 :line-height
 (fn [db _]
   "Get the line height for layout calculations"
   (:line-height db)))

;; -- Region/Mark Subscriptions --

(rf/reg-sub
 :mark-position
 :<- [:active-window]
 (fn [active-window _]
   "Get the mark position for the active window"
   (:mark-position active-window)))

(rf/reg-sub
 :region-active?
 :<- [:mark-position]
 (fn [mark-position _]
   "Check if a region is currently active (mark is set)"
   (not (nil? mark-position))))

;; -- Minibuffer Subscriptions --

(rf/reg-sub
 :minibuffer
 (fn [db _]
   "Get the minibuffer state"
   (:minibuffer db)))

;; -- Echo Area Subscriptions --

(rf/reg-sub
 :echo-area
 (fn [db _]
   "Get the echo area state"
   (:echo-area db)))

;; -- Parser and Highlighting Subscriptions --

(defn ast-to-decorations
  "Convert AST nodes to decoration maps for syntax highlighting"
  [ast]
  (when-let [children (:children ast)]
    (mapv (fn [node]
            (let [{:keys [type startPosition endPosition]} node
                  css-class (case type
                              "keyword" "syntax-keyword"
                              "string" "syntax-string"
                              "comment" "syntax-comment"
                              "number" "syntax-number"
                              "syntax-default")]
              {:from {:line (:row startPosition) :column (:column startPosition)}
               :to {:line (:row endPosition) :column (:column endPosition)}
               :class css-class
               :text (:text node)}))
          children)))

(rf/reg-sub
 :parser-worker-ready?
 (fn [db _]
   "Check if parser worker is ready"
   (get-in db [:system :parser-worker-ready?])))

(rf/reg-sub
 ::highlight-decorations
 :<- [:active-buffer]
 (fn [active-buffer _]
   "Get syntax highlighting decorations from AST"
   (let [ast (:ast active-buffer)]
     (if ast
       (ast-to-decorations ast)
       []))))

(defn lsp-diagnostics-to-decorations
  "Convert LSP diagnostics to decoration maps"
  [diagnostics]
  (mapv (fn [diagnostic]
          (let [{:keys [range severity message]} diagnostic
                {:keys [start end]} range
                css-class (case severity
                           1 "diagnostic-error"     ; Error
                           2 "diagnostic-warning"   ; Warning
                           3 "diagnostic-info"      ; Information
                           4 "diagnostic-hint"      ; Hint
                           "diagnostic-error")      ; Default to error
                severity-class (case severity
                               1 "severity-error"
                               2 "severity-warning"
                               3 "severity-info"
                               4 "severity-hint"
                               "severity-error")]
            {:from {:line (:line start) :column (:character start)}
             :to {:line (:line end) :column (:character end)}
             :class css-class
             :severity-class severity-class
             :message message
             :type :diagnostic}))
        diagnostics))

(rf/reg-sub
 ::diagnostic-decorations
 :<- [:active-buffer]
 (fn [active-buffer _]
   "Get diagnostic decorations from LSP diagnostics"
   (let [diagnostics (:diagnostics active-buffer [])
         decorations (lsp-diagnostics-to-decorations diagnostics)]
     (println "🔍 DIAGNOSTICS: Found" (count diagnostics) "diagnostics, generated" (count decorations) "decorations")
     (when (> (count diagnostics) 0)
       (println "🔍 DIAGNOSTICS: First diagnostic:" (first diagnostics))
       (println "🔍 DIAGNOSTICS: First decoration:" (first decorations)))
     decorations)))

(rf/reg-sub
 ::all-decorations
 :<- [::highlight-decorations]
 :<- [::diagnostic-decorations]
 (fn [[highlight-decorations diagnostic-decorations] _]
   "Merge syntax highlighting and diagnostic decorations"
   (concat highlight-decorations diagnostic-decorations)))

;; -- Parameterized Window Subscriptions (Phase 3B) --

(rf/reg-sub
 ::window-by-id
 :<- [:window-tree]
 (fn [window-tree [_ window-id]]
   "Get a specific window by ID"
   (db/find-window-in-tree window-tree window-id)))

(rf/reg-sub
 ::window-buffer
 (fn [[_ window-id]]
   [(rf/subscribe [::window-by-id window-id])
    (rf/subscribe [:buffers])])
 (fn [[window buffers] _]
   "Get the buffer for a specific window"
   (when window
     (get buffers (:buffer-id window)))))

(rf/reg-sub
 ::window-cursor-position
 (fn [[_ window-id]]
   (rf/subscribe [::window-by-id window-id]))
 (fn [window _]
   "Get cursor position for a specific window"
   (:cursor-position window)))

(rf/reg-sub
 ::window-mark-position
 (fn [[_ window-id]]
   (rf/subscribe [::window-by-id window-id]))
 (fn [window _]
   "Get mark position for a specific window"
   (:mark-position window)))

(rf/reg-sub
 ::window-viewport
 (fn [[_ window-id]]
   (rf/subscribe [::window-by-id window-id]))
 (fn [window _]
   "Get viewport for a specific window"
   (:viewport window)))

(rf/reg-sub
 ::window-visible-lines
 (fn [[_ window-id]]
   [(rf/subscribe [::window-viewport window-id])
    (rf/subscribe [::window-buffer window-id])])
 (fn [[viewport buffer] _]
   "Get visible lines for a specific window"
   (if (and buffer viewport)
     (let [{:keys [start-line end-line]} viewport
           full-text (get-in buffer [:cache :text] "")
           all-lines (clojure.string/split full-text #"\n" -1)
           visible-lines (subvec (vec all-lines) start-line (min (inc end-line) (count all-lines)))]
       (clojure.string/join "\n" visible-lines))
     "")))

(rf/reg-sub
 ::window-buffer-content
 (fn [[_ window-id]]
   (rf/subscribe [::window-buffer window-id]))
 (fn [buffer _]
   "Get buffer content for a specific window"
   (get-in buffer [:cache :text] "")))

(rf/reg-sub
 ::window-buffer-name
 (fn [[_ window-id]]
   (rf/subscribe [::window-buffer window-id]))
 (fn [buffer _]
   "Get buffer name for a specific window"
   (:name buffer)))

(rf/reg-sub
 ::window-decorations
 (fn [[_ window-id]]
   [(rf/subscribe [::window-buffer window-id])])
 (fn [[buffer] _]
   "Get decorations for a specific window's buffer"
   (let [ast (:ast buffer)
         diagnostics (:diagnostics buffer [])
         highlight-decorations (when ast (ast-to-decorations ast))
         diagnostic-decorations (lsp-diagnostics-to-decorations diagnostics)]
     (concat highlight-decorations diagnostic-decorations))))

;; -- Theme Subscriptions (Phase 7.8.1) --

(rf/reg-sub
 :current-theme
 (fn [db _]
   (:current-theme db :modus-vivendi)))

(rf/reg-sub
 :theme-face
 :<- [:current-theme]
 (fn [theme [_ face]]
   "Get style map for a face from current theme.

   Usage: @(rf/subscribe [:theme-face :mode-line])
   Returns: {:color '#ffffff' :background-color '#505050' ...}"
   (faces/face-to-style theme face)))
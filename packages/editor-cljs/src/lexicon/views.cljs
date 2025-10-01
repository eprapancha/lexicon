(ns lexicon.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [lexicon.constants :as const]
            [lexicon.subs :as subs]
            [lexicon.events :as events]))

;; -- Input Event Handling --

(defn handle-keydown
  "Handle keydown events - the core of our new Emacs-style input system"
  [event]
  (let [key-str (events/key-event-to-string event)]
    (when key-str
      ;; Prevent default browser behavior for bound keys
      (.preventDefault event)
      ;; Dispatch to our new key sequence handler
      (rf/dispatch [:handle-key-sequence key-str]))))

(defn handle-beforeinput
  "Handle beforeinput events - fallback for unbound printable characters"
  [event]
  (let [input-type (.-inputType event)
        data (.-data event)
        target (.-target event)
        selection (.getSelection js/window)
        cursor-pos (if (> (.-rangeCount selection) 0)
                     (.-startOffset (.getRangeAt selection 0))
                     0)]
    
    ;; Only handle insertText for characters not bound to commands
    (when (= input-type "insertText")
      ;; Prevent default browser behavior - we control all DOM mutations
      (.preventDefault event)
      
      ;; Dispatch to handle text input if it's a simple insertion
      (rf/dispatch [:handle-text-input {:input-type input-type
                                        :data data
                                        :dom-cursor-pos cursor-pos}]))))

(defn handle-composition-start
  "Handle IME composition start"
  [event]
  (rf/dispatch [:ime-composition-start]))

(defn handle-composition-update
  "Handle IME composition update"
  [event]
  (let [data (.-data event)]
    (rf/dispatch [:ime-composition-update data])))

(defn handle-composition-end
  "Handle IME composition end"
  [event]
  (let [data (.-data event)
        cursor-pos @(rf/subscribe [:cursor-position])]
    ;; Commit the final composed text
    (rf/dispatch [:ime-commit-composition data cursor-pos])
    (rf/dispatch [:ime-composition-end data])))

;; -- MutationObserver Safety Net --

(defn create-mutation-observer
  "Create MutationObserver for reconciliation safety net"
  [target-element]
  (let [observer (js/MutationObserver.
                  (fn [mutations]
                    ;; We can't use subscriptions in MutationObserver callbacks
                    ;; Just dispatch events and let the event handlers check state
                    (doseq [mutation mutations]
                      (when (= (.-type mutation) "childList")
                        ;; DOM mutation detected - this is normal during text input
                        ;; Dispatch reconciliation event - the handler will check if reconciliation is active
                        (rf/dispatch [:reconcile-dom-if-needed (.-textContent target-element)])))))]
    
    ;; Configure observer to watch for all changes
    (.observe observer target-element 
              #js {:childList true
                   :subtree true
                   :characterData true
                   :characterDataOldValue true})
    
    ;; Store observer reference
    (rf/dispatch [:set-mutation-observer observer])
    
    observer))

;; -- View Components --

(defn loading-view
  "Display loading state while WASM module loads"
  []
  [:div.loading
   [:div.loading-spinner]
   [:div.loading-text "Loading Lexicon..."]])

(defn handle-scroll
  "Handle scroll events for virtualized rendering"
  [event]
  (let [scroll-top (.-scrollTop (.-target event))
        line-height @(rf/subscribe [:line-height])
        lines-per-screen const/DEFAULT_VIEWPORT_LINES
        start-line (max 0 (js/Math.floor (/ scroll-top line-height)))
        end-line (+ start-line lines-per-screen)]
    (rf/dispatch [:update-viewport start-line end-line])))

(defn editor-view
  "Main virtualized editor view"
  []
  (let [visible-lines @(rf/subscribe [::subs/visible-lines])
        total-lines @(rf/subscribe [::subs/total-lines])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [::subs/viewport])
        ime-composing? @(rf/subscribe [:ime-composing?])
        ime-text @(rf/subscribe [:ime-composition-text])
        cursor-position @(rf/subscribe [:cursor-position])
        scroller-ref (atom nil)
        content-ref (atom nil)]
    
    (println "🖼️ Rendering editor. Visible lines:" (count (or visible-lines "")) "Total lines:" total-lines "Viewport:" viewport)
    
    (r/with-let [setup-scroller! (fn [element]
                                   (when element
                                     (reset! scroller-ref element)
                                     (.addEventListener element "scroll" handle-scroll)))
                 setup-content! (fn [element]
                                  (when element
                                    (reset! content-ref element)
                                    
                                    ;; Set up MutationObserver
                                    (create-mutation-observer element)
                                    
                                    ;; Set up event listeners
                                    (.addEventListener element "keydown" handle-keydown)
                                    (.addEventListener element "beforeinput" handle-beforeinput)
                                    (.addEventListener element "compositionstart" handle-composition-start)
                                    (.addEventListener element "compositionupdate" handle-composition-update)
                                    (.addEventListener element "compositionend" handle-composition-end)))
                 cleanup-element! (fn []
                                    (when-let [scroller @scroller-ref]
                                      (.removeEventListener scroller "scroll" handle-scroll))
                                    (when-let [content @content-ref]
                                      (.removeEventListener content "keydown" handle-keydown)
                                      (.removeEventListener content "beforeinput" handle-beforeinput)
                                      (.removeEventListener content "compositionstart" handle-composition-start)
                                      (.removeEventListener content "compositionupdate" handle-composition-update)
                                      (.removeEventListener content "compositionend" handle-composition-end))
                                    
                                    ;; Disconnect MutationObserver
                                    (when-let [observer @(rf/subscribe [:mutation-observer])]
                                      (.disconnect observer)))]
      
      ;; Update cursor position when it changes
      (r/track! (fn []
                  (let [current-cursor @(rf/subscribe [:cursor-position])]
                    (when-let [content @content-ref]
                      (when current-cursor
                        (js/setTimeout 
                         (fn []
                           (try
                             ;; Find the first line div and its text node
                             (let [first-line-div (.-firstElementChild content)]
                               (when first-line-div
                                 (let [text-node (.-firstChild first-line-div)]
                                   (when text-node
                                     (let [selection (.getSelection js/window)
                                           range (.createRange js/document)
                                           max-pos (.-length text-node)
                                           safe-pos (min current-cursor max-pos)]
                                       (println "🎯 Setting cursor to position:" safe-pos "in text:" (pr-str (.-textContent text-node)))
                                       (.setStart range text-node safe-pos)
                                       (.setEnd range text-node safe-pos)
                                       (.removeAllRanges selection)
                                       (.addRange selection range))))))
                             (catch js/Error e
                               (println "Error setting cursor position:" e))))
                         10))))))
      
      [:div.editor-container
       ;; Scroller div - creates the scrollbar based on total document height
       [:div.editor-scroller
        {:ref setup-scroller!
         :style {:height "100vh"
                 :overflow-y "auto"
                 :position "relative"
                 :background-color "#1e1e1e"}}
        
        ;; Virtual space to create proper scrollbar height
        [:div.virtual-space
         {:style {:height (str (* total-lines line-height) "px")
                  :position "relative"}}
         
         ;; Content div - absolutely positioned to show visible content
         [:div.editor-content
          {:ref setup-content!
           :contentEditable true
           :suppressContentEditableWarning true
           :style {:position "absolute"
                   :top (str (* (:start-line viewport 0) line-height) "px")
                   :left "0"
                   :right "0"
                   :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
                   :font-size "14px"
                   :line-height (str line-height "px")
                   :padding "20px"
                   :outline "none"
                   :white-space "pre-wrap"
                   :color "#d4d4d4"
                   :border "none"
                   :min-height "auto"}}
          
          ;; Render visible lines as individual divs
          (when visible-lines
            (let [lines (clojure.string/split visible-lines #"\n")]
              (for [[idx line] (map-indexed vector lines)]
                ^{:key (+ (:start-line viewport 0) idx)}
                [:div.line
                 {:style {:min-height (str line-height "px")}}
                 line])))
          
          ;; Show IME composition text if active
          (when ime-composing?
            [:span.ime-composition
             {:style {:background-color "rgba(255, 255, 0, 0.3)"
                      :text-decoration "underline"}}
             ime-text])]]]])))

(defn buffer-tab
  "Display a single buffer tab with close button"
  [buffer is-active?]
  [:div.buffer-tab
   {:style {:display "inline-flex"
            :align-items "center"
            :padding "4px 8px"
            :margin-right "4px"
            :background-color (if is-active? "#4e4e50" "#2d2d30")
            :border "1px solid #3e3e42"
            :border-radius "4px 4px 0 0"
            :font-size "12px"
            :font-family "monospace"}}
   
   [:span.buffer-name
    {:style {:margin-right "6px"}}
    (str (:name buffer) (when (:is-modified? buffer) " •"))]
   
   [:button.buffer-close
    {:style {:background "none"
             :border "none"
             :color "#cccccc"
             :cursor "pointer"
             :padding "0"
             :margin "0"
             :font-size "14px"
             :line-height "1"
             :width "16px"
             :height "16px"
             :display "flex"
             :align-items "center"
             :justify-content "center"}
     :on-click (fn [e]
                 (.stopPropagation e)
                 (rf/dispatch [:close-buffer (:id buffer)]))}
    "×"]])

(defn buffer-tabs
  "Display all open buffer tabs"
  []
  (let [buffers @(rf/subscribe [:buffers])
        active-buffer @(rf/subscribe [:active-buffer])]
    
    [:div.buffer-tabs
     {:style {:position "fixed"
              :top "0"
              :left "0"
              :right "0"
              :height "32px"
              :background-color "#2d2d30"
              :border-bottom "1px solid #3e3e42"
              :display "flex"
              :align-items "flex-end"
              :padding "0 10px"
              :overflow-x "auto"}}
     
     (for [buffer (vals buffers)]
       ^{:key (:id buffer)}
       [buffer-tab buffer (= (:id buffer) (:id active-buffer))])]))

(defn status-bar
  "Display editor status information"
  []
  (let [cursor-pos @(rf/subscribe [:cursor-position])
        buffer-length @(rf/subscribe [:buffer-length])
        buffer-modified? @(rf/subscribe [:buffer-modified?])
        active-buffer @(rf/subscribe [:active-buffer])]
    
    [:div.status-bar
     {:style {:position "fixed"
              :bottom "0"
              :left "0"
              :right "0"
              :height "24px"
              :background-color "#2d2d30"
              :color "#cccccc"
              :font-size "12px"
              :font-family "monospace"
              :display "flex"
              :align-items "center"
              :padding "0 10px"
              :border-top "1px solid #3e3e42"}}
     
     [:span.buffer-info
      (str (:name active-buffer) (when buffer-modified? " •"))]
     
     [:div.spacer {:style {:flex "1"}}]
     
     [:span.cursor-info
      (str "Pos: " cursor-pos " | Len: " buffer-length)]]))

(defn main-app
  "Main application component"
  []
  (let [initialized? @(rf/subscribe [:initialized?])
        editor-ready? @(rf/subscribe [:editor-ready?])
        wasm-error @(rf/subscribe [:wasm-error])]
    
    [:div.lexicon-app
     {:style {:width "100%"
              :height "100vh"
              :display "flex"
              :flex-direction "column"}}
     
     (cond
       wasm-error
       [:div.error
        {:style {:padding "20px"
                 :color "#ff6b6b"
                 :background-color "#2d1b1b"
                 :border "1px solid #ff6b6b"
                 :border-radius "4px"
                 :margin "20px"
                 :font-family "monospace"}}
        [:h3 "WASM Loading Error"]
        [:p "Failed to load the WebAssembly module:"]
        [:pre {:style {:background-color "#1a1a1a"
                       :padding "10px"
                       :border-radius "4px"
                       :overflow-x "auto"}}
         (str wasm-error)]
        [:p "Please check the browser console for more details."]]
       
       (not initialized?)
       [loading-view]
       
       editor-ready?
       [:<>
        [editor-view]
        [status-bar]]
       
       :else
       [:div.error "Failed to initialize editor"])]))

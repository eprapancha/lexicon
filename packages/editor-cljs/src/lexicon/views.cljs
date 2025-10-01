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
    (println "⌨️ keydown event:" key-str)
    (when key-str
      ;; Prevent default browser behavior for bound keys
      (.preventDefault event)
      ;; Dispatch to our new key sequence handler
      (rf/dispatch [:handle-key-sequence key-str]))))

(defn handle-beforeinput
  "Handle beforeinput events - fallback for unbound printable characters"
  [event]
  (println "📝 beforeinput event:" (.-inputType event) "data:" (.-data event))
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

;; -- Hidden Textarea + Custom DOM Architecture --

(defn hidden-input-handler
  "Hidden textarea that captures all user input"
  [input-ref-atom]
  [:textarea.hidden-input
   {:ref (fn [element]
           (when element
             (println "📱 Hidden textarea element created")
             (reset! input-ref-atom element)
             (.focus element)
             (println "📱 Hidden textarea focused")))
    :on-key-down handle-keydown
    :on-input handle-beforeinput
    :on-before-input handle-beforeinput
    :on-paste (fn [e]
                (.preventDefault e)
                (let [paste-data (-> e .-clipboardData (.getData "text"))]
                  (rf/dispatch [:editor/paste-text paste-data])))
    :on-composition-start handle-composition-start
    :on-composition-update handle-composition-update
    :on-composition-end handle-composition-end
    :style {:position "absolute"
            :top "0"
            :left "0"
            :width "200px"
            :height "100px"
            :opacity "0"
            :background-color "transparent"
            :caret-color "transparent"
            :resize "none"
            :border "none"
            :outline "none"
            :z-index "9999"}}])

(defn custom-rendered-pane
  "Read-only text display using divs per line"
  []
  (let [visible-lines @(rf/subscribe [::subs/visible-lines])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [::subs/viewport])]
    
    [:div.text-pane
     {:style {:position "absolute"
              :top (str (* (:start-line viewport 0) line-height) "px")
              :left "0"
              :right "0"
              :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
              :font-size "14px"
              :line-height (str line-height "px")
              :padding "20px"
              :color "#d4d4d4"
              :white-space "pre-wrap"
              :pointer-events "none"
              :background-color "#1e1e1e"}}  ; Same as main background
     
     ;; Inner editable content area with subtle background
     [:div.editable-area
      {:style {:background-color "rgba(37, 37, 38, 0.5)"  ; Semi-transparent for debugging
               :border-radius "4px"
               :min-height "200px"
               :position "relative"
               :padding "8px"
               :z-index "1"}}  ; Ensure it's layered properly
      
      ;; Render visible lines as individual divs
      (when visible-lines
        (let [lines (clojure.string/split visible-lines #"\n")]
          (for [[idx line] (map-indexed vector lines)]
            ^{:key (+ (:start-line viewport 0) idx)}
            [:div.text-line
             {:style {:min-height (str line-height "px")
                      :position "relative"
                      :z-index "2"
                      :color "#d4d4d4"}}  ; Ensure text color is set
             [:span.line-content 
              {:style {:color "#d4d4d4"}}  ; Explicit text color
              line]])))]]))

(defn custom-cursor
  "Custom cursor element positioned by our application state"
  []
  (let [cursor-pos @(rf/subscribe [::subs/cursor-position])
        line-height @(rf/subscribe [:line-height])]
    
    (println "🎯 Custom cursor render - cursor-pos:" cursor-pos "line-height:" line-height)
    
    (if cursor-pos
      (let [{:keys [line column]} cursor-pos
            ;; Calculate pixel position  
            char-width 8.4  ; Approximate monospace char width - TODO: measure dynamically
            top-px (* line line-height)
            left-px (+ 20 (* column char-width))]  ; 20px for padding
        
        (println "🎯 Rendering cursor at line:" line "column:" column "top:" top-px "left:" left-px)
        
        [:div.custom-cursor
         {:style {:position "absolute"
                  :top (str (+ top-px 20 8) "px")    ; Add 20px outer + 8px inner padding
                  :left (str (+ left-px 8) "px")     ; Add 8px inner left padding
                  :width "2px"
                  :height (str line-height "px")
                  :background-color "#ffffff"
                  :pointer-events "none"
                  :animation "cursor-blink 1s infinite"
                  :z-index "1000"}}])
      (do
        (println "❌ Custom cursor - no cursor-pos data!")
        ;; Render a fallback cursor at 0,0 so we can see if the element is there
        [:div.custom-cursor-fallback
         {:style {:position "absolute"
                  :top "0px"
                  :left "20px"
                  :width "2px" 
                  :height (str line-height "px")
                  :background-color "#ff0000"  ; Red so we can see it
                  :pointer-events "none"
                  :z-index "1000"}}]))))

(defn editor-wrapper
  "Main editor wrapper with hidden textarea + custom DOM architecture"
  []
  (let [hidden-input-ref (atom nil)]
    
    [:div.editor-wrapper
     {:tabIndex 0
      :on-click (fn [_]
                  (println "🖱️ Editor wrapper clicked")
                  ;; Focus the hidden input when wrapper is clicked
                  (when-let [hidden-input @hidden-input-ref]
                    (println "🎯 Focusing hidden input")
                    (.focus hidden-input))
                  ;; TODO: Dispatch click-to-cursor event
                  )
      :style {:position "relative"
              :width "100%"
              :height "100%"
              :outline "none"}}
     
     ;; Hidden input handler - captures all keyboard input
     [hidden-input-handler hidden-input-ref]
     
     ;; Custom rendered pane - displays text content
     [custom-rendered-pane]
     
     ;; Custom cursor - fake cursor controlled by our state
     [custom-cursor]]))

(defn editor-view
  "Main editor view with virtualized scrolling and custom cursor architecture"
  []
  (let [total-lines @(rf/subscribe [::subs/total-lines])
        line-height @(rf/subscribe [:line-height])
        scroller-ref (atom nil)]
    
    [:div.editor-container
     ;; Scroller div - creates the scrollbar based on total document height
     [:div.editor-scroller
      {:ref (fn [element]
              (when element
                (reset! scroller-ref element)
                (.addEventListener element "scroll" handle-scroll)))
       :style {:height "calc(100vh - 56px)"  ; Account for buffer tabs and status bar
               :overflow-y "auto"
               :position "relative"
               :background-color "#1e1e1e"
               :margin-top "32px"}}
      
      ;; Virtual space to create proper scrollbar height
      [:div.virtual-space
       {:style {:height (str (* total-lines line-height) "px")
                :position "relative"}}
       
       ;; New editor wrapper with custom architecture
       [editor-wrapper]]]]))

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
  (let [initialized?  @(rf/subscribe [:initialized?])
        editor-ready? @(rf/subscribe [:editor-ready?])
        wasm-error    @(rf/subscribe [:wasm-error])]
    
    [:<>
     ;; Add CSS for cursor animation
     [:style
      "@keyframes cursor-blink {
         0%, 50% { opacity: 1; }
         51%, 100% { opacity: 0; }
       }"]
     
     [:div.lexicon-app
      {:style {:width          "100%"
               :height         "100vh"
               :display        "flex"
               :flex-direction "column"}}
      
      (cond
        wasm-error
        [:div.error
         {:style {:padding          "20px"
                  :color            "#ff6b6b"
                  :background-color "#2d1b1b"
                  :border           "1px solid #ff6b6b"
                  :border-radius    "4px"
                  :margin           "20px"
                  :font-family      "monospace"}}
         [:h3 "WASM Loading Error"]
         [:p "Failed to load the WebAssembly module:"]
         [:pre {:style {:background-color "#1a1a1a"
                        :padding          "10px"
                        :border-radius    "4px"
                        :overflow-x       "auto"}}
          (str wasm-error)]
         [:p "Please check the browser console for more details."]]
        
        (not initialized?)
        [loading-view]
        
        editor-ready?
        [:<>
         [buffer-tabs]
         [editor-view]
         [status-bar]]
        
        :else
        [:div.error "Failed to initialize editor"])]]))

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
            :width "100%"
            :height "100%"
            :opacity "0"
            :background-color "transparent"
            :caret-color "transparent"
            :resize "none"
            :border "none"
            :outline "none"
            :pointer-events "none"  ; Let clicks pass through to elements below
            :z-index "100"}}])

(defn apply-decorations-to-line
  "Apply syntax highlighting decorations to a line of text"
  [line-text line-number decorations]
  (let [line-decorations (filter #(= (:line (:from %)) line-number) decorations)
        line-diagnostics (filter #(= (:type %) :diagnostic) line-decorations)]
    ; (when (= line-number 15)
    ;   (println "🎨 LINE 15: Found" (count line-decorations) "total decorations," (count line-diagnostics) "diagnostic decorations")
    ;   (when (> (count line-diagnostics) 0)
    ;     (println "🎨 LINE 15: First diagnostic decoration:" (first line-diagnostics))))
    (if (empty? line-decorations)
      ;; No decorations, return plain text
      [:span.line-content 
       {:style {:color "#d4d4d4"}}
       line-text]
      ;; Apply decorations
      (let [segments (atom [])
            current-pos (atom 0)]
        
        ;; Sort decorations by column position
        (doseq [decoration (sort-by #(get-in % [:from :column]) line-decorations)]
          (let [start-col (get-in decoration [:from :column])
                end-col (get-in decoration [:to :column])
                text-before (subs line-text @current-pos start-col)
                decorated-text (subs line-text start-col end-col)]
            
            ;; Add text before decoration
            (when (seq text-before)
              (swap! segments conj [:span text-before]))
            
            ;; Add decorated text with attributes for diagnostic decorations
            (let [span-attrs (cond-> {:class (:class decoration)}
                               (:message decoration) (assoc :data-message (:message decoration)))]
              ; (when (= (:type decoration) :diagnostic)
              ;   (println "🎨 DECORATION: Creating diagnostic span with attrs:" span-attrs "text:" (pr-str decorated-text)))
              (swap! segments conj [:span span-attrs decorated-text]))
            
            ;; Update position
            (reset! current-pos end-col)))
        
        ;; Add remaining text
        (let [remaining-text (subs line-text @current-pos)]
          (when (seq remaining-text)
            (swap! segments conj [:span remaining-text])))
        
        ;; Return combined segments
        (into [:span.line-content {:style {:color "#d4d4d4"}}] @segments)))))

(defn editor-gutter
  "IDE-style gutter with line numbers and diagnostic markers"
  []
  (let [visible-lines @(rf/subscribe [:lexicon.subs/visible-lines])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [:lexicon.subs/viewport])
        decorations @(rf/subscribe [:lexicon.subs/all-decorations])
        diagnostic-decorations (filter #(= (:type %) :diagnostic) decorations)]

    [:div.gutter
     {:style {:flex-shrink "0"
              :width "60px"
              :background-color "#2d2d30"
              :border-right "1px solid #3e3e3e"
              :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
              :font-size "12px"
              :line-height (str line-height "px")
              :color "#858585"
              :user-select "none"
              :padding-top "20px"}}
     
     ;; Render line numbers and markers
     (when visible-lines
       (let [lines (clojure.string/split visible-lines #"\n" -1)  ; -1 keeps trailing empty strings
             start-line (:start-line viewport 0)]
         (map-indexed
           (fn [idx line]
             (let [line-num (+ start-line idx 1)
                   line-diagnostics (filter #(= (:line (:from %)) (dec line-num)) diagnostic-decorations)
                   has-error? (some #(= (:class %) "diagnostic-error") line-diagnostics)
                   has-warning? (some #(= (:class %) "diagnostic-warning") line-diagnostics)
                   has-hint? (some #(= (:class %) "diagnostic-hint") line-diagnostics)]
               [:div.gutter-line
                {:key line-num
                 :style {:height (str line-height "px")
                         :display "flex"
                         :align-items "center"
                         :padding-right "8px"
                         :position "relative"}}
                
                ;; Diagnostic marker
                (when (or has-error? has-warning? has-hint?)
                  [:div.diagnostic-marker
                   {:style {:position "absolute"
                            :left "4px"
                            :width "8px"
                            :height "8px"
                            :border-radius "50%"
                            :background-color (cond
                                               has-error? "#f14c4c"
                                               has-warning? "#ff8c00"
                                               has-hint? "#d7ba7d"
                                               :else "#666")}}])
                
                ;; Line number
                [:div.line-number
                 {:style {:margin-left "auto"
                          :text-align "right"
                          :font-weight (if (or has-error? has-warning? has-hint?) "bold" "normal")
                          :color (cond
                                  has-error? "#f14c4c"
                                  has-warning? "#ff8c00"
                                  has-hint? "#d7ba7d"
                                  :else "#858585")}}
                 line-num]]))
           lines)))]))

(defn custom-rendered-pane
  "Read-only text display using divs per line with syntax highlighting"
  [hidden-input-ref]
  (let [visible-lines @(rf/subscribe [:lexicon.subs/visible-lines])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [:lexicon.subs/viewport])
        decorations @(rf/subscribe [:lexicon.subs/all-decorations])
        cursor-pos @(rf/subscribe [:lexicon.subs/cursor-position])
        handle-click (fn [e]
                       (println "📍 Text container clicked!")
                       (.stopPropagation e)  ; Stop event from bubbling to parent handlers
                       ;; Focus hidden input to receive keyboard events
                       (when-let [hidden-input @hidden-input-ref]
                         (.focus hidden-input))
                       (let [rect (-> e .-currentTarget .getBoundingClientRect)
                             click-x (- (.-clientX e) (.-left rect))
                             click-y (- (.-clientY e) (.-top rect))
                             char-width 8.4
                             left-padding 8
                             top-padding 20
                             ;; Calculate line and column from click position
                             clicked-line (int (/ (- click-y top-padding) line-height))
                             clicked-column (max 0 (int (/ (- click-x left-padding) char-width)))
                             ;; Add viewport offset to get absolute line number
                             absolute-line (+ clicked-line (:start-line viewport 0))]
                         (println "🖱️ Click at pixel" click-x "," click-y
                                  "→ line" absolute-line "col" clicked-column)
                         (rf/dispatch [:click-to-position absolute-line clicked-column])))]

    [:div.text-container
     {:style {:flex "1"
              :position "relative"
              :overflow "hidden"}
      :on-click handle-click}

     [:div.editable-area
      {:style {:background-color "rgba(37, 37, 38, 0.5)"  ; Semi-transparent highlight
               :border-radius "4px"
               :position "relative"
               :padding "20px 8px"  ; Top/bottom 20px, left/right 8px
               :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
               :font-size "14px"
               :line-height (str line-height "px")
               :color "#d4d4d4"
               :white-space "pre-wrap"
               :pointer-events "auto"  ; Enable clicks for cursor positioning
               :min-height (str line-height "px")}}  ; At least one line tall

      ;; Render visible lines as individual divs with syntax highlighting
      (when visible-lines
        (let [lines (clojure.string/split visible-lines #"\n" -1)]  ; -1 keeps trailing empty strings
          (for [[idx line] (map-indexed vector lines)]
            (let [line-number (+ (:start-line viewport 0) idx)]
              ^{:key line-number}
              [:div.text-line
               {:style {:min-height (str line-height "px")
                        :color "#d4d4d4"}}
               (apply-decorations-to-line line line-number decorations)]))))

      ;; Cursor - now positioned INSIDE editable-area
      (when cursor-pos
        (let [{:keys [line column]} cursor-pos
              char-width 8.4
              left-padding 8  ; Account for editable-area left padding
              top-padding 20  ; Account for editable-area top padding
              top-px (+ top-padding (* line line-height))
              left-px (+ left-padding (* column char-width))]
          [:div.custom-cursor
           {:style {:position "absolute"
                    :top (str top-px "px")
                    :left (str left-px "px")
                    :width "2px"
                    :height (str line-height "px")
                    :background-color "#ffffff"
                    :pointer-events "none"
                    :animation "cursor-blink 1s infinite"
                    :z-index "1000"}}]))]]))


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
              :min-height "100%"
              :outline "none"}}
     
     ;; Hidden input handler - captures all keyboard input
     [hidden-input-handler hidden-input-ref]
     
     
     ;; Flexbox container for gutter + text
     [:div.editor-content
      {:on-click (fn [e] (when-let [hidden-input @hidden-input-ref] (.focus hidden-input)) (.stopPropagation e)) :style {:display "flex" :flex-direction "row" :width "100%" :min-height "100%"}}
      [editor-gutter]
      [custom-rendered-pane hidden-input-ref]]]))

(defn editor-view
  "Main editor view with virtualized scrolling and custom cursor architecture"
  []
  (let [total-lines @(rf/subscribe [:lexicon.subs/total-lines])
        line-height @(rf/subscribe [:line-height])
        scroller-ref (atom nil)]
    
    [:div.editor-container
     ;; Scroller div - creates the scrollbar based on total document height
     [:div.editor-scroller
      {:ref (fn [element]
              (when element
                (reset! scroller-ref element)
                (.addEventListener element "scroll" handle-scroll)))
       :style {:height "calc(100vh - 24px)"  ; Account for status bar only
               :overflow-y "auto"
               :position "relative"
               :background-color "#1e1e1e"}}
      
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

(defn minibuffer-view
  "Minibuffer component for interactive commands (M-x style)"
  []
  (let [minibuffer @(rf/subscribe [:minibuffer])
        input-ref (atom nil)]
    
    (when (:active? minibuffer)
      [:div.minibuffer
       {:style {:position "fixed"
                :bottom "0"
                :left "0"
                :right "0"
                :height "32px"
                :background-color "#1e1e1e"
                :color "#cccccc"
                :font-size "14px"
                :font-family "monospace"
                :display "flex"
                :align-items "center"
                :padding "0 10px"
                :border-top "1px solid #3e3e42"
                :z-index "1000"}}
       
       [:span.minibuffer-prompt
        {:style {:margin-right "4px"
                 :color "#569cd6"}}
        (:prompt minibuffer)]
       
       [:input.minibuffer-input
        {:ref (fn [element]
                (when element
                  (reset! input-ref element)
                  (.focus element)))
         :type "text"
         :value (:input minibuffer)
         :on-change (fn [e]
                      (rf/dispatch [:minibuffer/set-input (.. e -target -value)]))
         :on-key-down (fn [e]
                        (let [key (.-key e)]
                          (cond
                            (= key "Enter")
                            (do
                              (.preventDefault e)
                              (rf/dispatch [:minibuffer/confirm]))
                            
                            (or (= key "Escape") 
                                (and (.-ctrlKey e) (= key "g")))
                            (do
                              (.preventDefault e)
                              (rf/dispatch (:on-cancel minibuffer))))))
         :style {:background-color "transparent"
                 :border "none"
                 :outline "none"
                 :color "#cccccc"
                 :font-size "14px"
                 :font-family "monospace"
                 :flex "1"}}]])))

(defn status-bar
  "Display editor status information"
  []
  (let [cursor-pos @(rf/subscribe [:lexicon.subs/cursor-position])
        buffer-length @(rf/subscribe [:buffer-length])
        buffer-modified? @(rf/subscribe [:buffer-modified?])
        active-buffer @(rf/subscribe [:active-buffer])
        minibuffer @(rf/subscribe [:minibuffer])]

    (when-not (:active? minibuffer)
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
        (when cursor-pos
          (str "Line " (inc (:line cursor-pos)) ", Col " (:column cursor-pos) " | " buffer-length " chars"))]])))

(defn main-app
  "Main application component"
  []
  (let [initialized?  @(rf/subscribe [:initialized?])
        editor-ready? @(rf/subscribe [:editor-ready?])
        wasm-error    @(rf/subscribe [:wasm-error])]
    
    [:<>
     ;; Add CSS for cursor animation and syntax highlighting
     [:style
      "@keyframes cursor-blink {
         0%, 50% { opacity: 1; }
         51%, 100% { opacity: 0; }
       }
       .syntax-keyword { color: #569cd6; font-weight: bold; }
       .syntax-string { color: #ce9178; }
       .syntax-comment { color: #6a9955; font-style: italic; }
       .syntax-number { color: #b5cea8; }
       .syntax-default { color: #d4d4d4; }"]
     
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
         [editor-view]
         [status-bar]
         [minibuffer-view]]
        
        :else
        [:div.error "Failed to initialize editor"])]]))

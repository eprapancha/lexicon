(ns lexicon.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

;; -- Input Event Handling --

(defn handle-beforeinput
  "Handle beforeinput events - the core of our proactive input strategy"
  [event]
  (let [input-type (.-inputType event)
        data (.-data event)
        target (.-target event)
        selection (.getSelection js/window)
        cursor-pos (if (> (.-rangeCount selection) 0)
                     (.-startOffset (.getRangeAt selection 0))
                     0)]
    
    ;; Prevent default browser behavior - we control all DOM mutations
    (.preventDefault event)
    
    ;; Translate input type to transaction
    (case input-type
      "insertText"
      (when data
        (rf/dispatch [:dispatch-transaction {:type :insert
                                             :pos cursor-pos
                                             :text data}]))
      
      "insertCompositionText"
      ;; Handle during IME composition
      (when data
        (rf/dispatch [:ime-composition-update data]))
      
      "deleteContentBackward"
      (when (> cursor-pos 0)
        (rf/dispatch [:dispatch-transaction {:type :delete
                                             :pos (dec cursor-pos)
                                             :length 1}]))
      
      "deleteContentForward"
      (rf/dispatch [:dispatch-transaction {:type :delete
                                           :pos cursor-pos
                                           :length 1}])
      
      "deleteByCut"
      (let [selection-start (.-anchorOffset selection)
            selection-end (.-focusOffset selection)
            delete-start (min selection-start selection-end)
            delete-length (abs (- selection-end selection-start))]
        (when (> delete-length 0)
          (rf/dispatch [:dispatch-transaction {:type :delete
                                               :pos delete-start
                                               :length delete-length}])))
      
      "insertFromPaste"
      (when data
        (rf/dispatch [:dispatch-transaction {:type :insert
                                             :pos cursor-pos
                                             :text data}]))
      
      ;; Log unhandled input types for debugging
      (println "Unhandled input type:" input-type))))

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
                    (let [reconciliation-active? @(rf/subscribe [:reconciliation-active?])]
                      ;; Only process mutations if we're not in the middle of reconciliation
                      (when-not reconciliation-active?
                        (doseq [mutation mutations]
                          (when (= (.-type mutation) "childList")
                            (println "⚠️ Unexpected DOM mutation detected")
                            (let [dom-content (.-textContent target-element)
                                  wasm-handle @(rf/subscribe [:wasm-handle])]
                              (when wasm-handle
                                (let [expected-content (.getText wasm-handle)]
                                  (when (not= dom-content expected-content)
                                    (println "🔄 Reconciling DOM state")
                                    (rf/dispatch [:reconcile-dom-state dom-content expected-content])))))))
                    ;; Store observer reference
                    (rf/dispatch [:set-mutation-observer observer]))))]
    
    ;; Configure observer to watch for all changes
    (.observe observer target-element 
              #js {:childList true
                   :subtree true
                   :characterData true
                   :characterDataOldValue true})
    
    observer))

;; -- View Components --

(defn loading-view
  "Display loading state while WASM module loads"
  []
  [:div.loading
   [:div.loading-spinner]
   [:div.loading-text "Loading Lexicon..."]])

(defn editor-view
  "Main contenteditable editor view"
  []
  (let [content @(rf/subscribe [:buffer-content])
        ime-composing? @(rf/subscribe [:ime-composing?])
        ime-text @(rf/subscribe [:ime-composition-text])
        view-needs-update? @(rf/subscribe [:view-needs-update?])
        editor-ref (r/atom nil)]
    
    (r/create-class
     {:component-did-mount
      (fn [this]
        (let [element (r/dom-node this)]
          (reset! editor-ref element)
          
          ;; Set up MutationObserver
          (create-mutation-observer element)
          
          ;; Set up event listeners
          (.addEventListener element "beforeinput" handle-beforeinput)
          (.addEventListener element "compositionstart" handle-composition-start)
          (.addEventListener element "compositionupdate" handle-composition-update)
          (.addEventListener element "compositionend" handle-composition-end)))
      
      :component-will-unmount
      (fn [this]
        (when-let [element @editor-ref]
          (.removeEventListener element "beforeinput" handle-beforeinput)
          (.removeEventListener element "compositionstart" handle-composition-start)
          (.removeEventListener element "compositionupdate" handle-composition-update)
          (.removeEventListener element "compositionend" handle-composition-end))
        
        ;; Disconnect MutationObserver
        (when-let [observer @(rf/subscribe [:mutation-observer])]
          (.disconnect observer)))
      
      :component-did-update
      (fn [this old-argv]
        ;; Update DOM content when view needs update
        (when view-needs-update?
          (let [element @editor-ref]
            (rf/dispatch [:set-reconciliation-active true])
            
            ;; Temporarily disconnect MutationObserver
            (when-let [observer @(rf/subscribe [:mutation-observer])]
              (.disconnect observer))
            
            ;; Update content
            (set! (.-textContent element) content)
            
            ;; Reconnect MutationObserver
            (when-let [observer @(rf/subscribe [:mutation-observer])]
              (.observe observer element 
                        #js {:childList true
                             :subtree true
                             :characterData true
                             :characterDataOldValue true}))
            
            (rf/dispatch [:set-reconciliation-active false])
            (rf/dispatch [:view-updated]))))
      
      :reagent-render
      (fn []
        [:div.editor-container
         [:div.editor-content
          {:contentEditable true
           :suppressContentEditableWarning true
           :style {:min-height "100vh"
                   :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
                   :font-size "14px"
                   :line-height "1.5"
                   :padding "20px"
                   :outline "none"
                   :white-space "pre-wrap"
                   :background-color "#1e1e1e"
                   :color "#d4d4d4"
                   :border "none"}}
          content
          
          ;; Show IME composition text if active
          (when ime-composing?
            [:span.ime-composition
             {:style {:background-color "rgba(255, 255, 0, 0.3)"
                      :text-decoration "underline"}}
             ime-text])]])})))

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
      (str (:id active-buffer) (when buffer-modified? " •"))]
     
     [:div.spacer {:style {:flex "1"}}]
     
     [:span.cursor-info
      (str "Pos: " cursor-pos " | Len: " buffer-length)]]))

(defn main-app
  "Main application component"
  []
  (let [initialized? @(rf/subscribe [:initialized?])
        editor-ready? @(rf/subscribe [:editor-ready?])]
    
    [:div.lexicon-app
     {:style {:width "100%"
              :height "100vh"
              :display "flex"
              :flex-direction "column"}}
     
     (cond
       (not initialized?)
       [loading-view]
       
       editor-ready?
       [:<>
        [editor-view]
        [status-bar]]
       
       :else
       [:div.error "Failed to initialize editor"])]))
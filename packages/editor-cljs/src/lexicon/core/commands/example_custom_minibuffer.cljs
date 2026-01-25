(ns lexicon.core.commands.example-custom-minibuffer
  "Example command demonstrating custom minibuffer renderer (Issue #46).

  This is a reference implementation showing how to use the custom renderer API
  to create Vertico/Consult-style UIs with preview panes and custom layouts."
  (:require [re-frame.core :as rf]
            [lexicon.core.minibuffer :as mb]))

;; =============================================================================
;; Custom Renderer Component
;; =============================================================================

(defn fancy-file-selector-renderer
  "Custom minibuffer renderer with two-column layout.

  Left column: File list with icons
  Right column: File preview/info

  Props:
  - :minibuffer - Full minibuffer state
  - :mode-line-style - Theme styling
  - :prompt-style - Prompt styling
  - :file-info - Custom prop with file metadata"
  [{:keys [minibuffer mode-line-style file-info]}]
  (let [input (:input minibuffer)
        candidates (:filtered-completions minibuffer)
        selected-idx (:completion-index minibuffer)
        selected-file (when (< selected-idx (count candidates))
                        (nth candidates selected-idx))]
    [:div.fancy-file-selector
     {:style {:display "flex"
              :height "100%"
              :gap "8px"}}

     ;; Left: File list
     [:div.file-list
      {:style {:flex "1"
               :overflow-y "auto"
               :border-right (str "1px solid " (:border-color mode-line-style "#3e3e42"))}}
      (if (empty? candidates)
        [:div.no-matches
         {:style {:padding "8px"
                  :color "#888"
                  :font-style "italic"}}
         "No matches"]

        (for [[idx file] (map-indexed vector (take 20 candidates))]
          ^{:key idx}
          [:div.file-item
           {:style {:padding "6px 8px"
                    :cursor "pointer"
                    :background-color (when (= idx selected-idx)
                                        "rgba(100, 150, 255, 0.3)")
                    :display "flex"
                    :align-items "center"
                    :gap "8px"}
            :on-click #(rf/dispatch [:minibuffer/select-completion idx])}
           [:span.file-icon "📄"]
           [:span.file-name file]]))]

     ;; Right: Preview pane
     [:div.preview-pane
      {:style {:flex "1"
               :padding "8px"
               :overflow-y "auto"}}
      (if selected-file
        [:div
         [:div.preview-header
          {:style {:font-weight "bold"
                   :margin-bottom "8px"
                   :padding-bottom "4px"
                   :border-bottom "1px solid #444"}}
          "File Info"]

         [:div.preview-content
          [:div {:style {:margin-bottom "4px"}}
           [:strong "Name: "] selected-file]
          [:div {:style {:margin-bottom "4px"}}
           [:strong "Type: "] (or (:type file-info) "Unknown")]
          [:div {:style {:margin-bottom "4px"}}
           [:strong "Size: "] (or (:size file-info) "N/A")]
          [:div {:style {:margin-top "12px" :color "#888"}}
           "This is a demo preview pane. In a real implementation, this would show file contents, metadata, or other relevant information."]]]

        [:div.no-selection
         {:style {:color "#888"
                  :font-style "italic"}}
         "Select a file to see preview"])]]))

;; =============================================================================
;; Example Command
;; =============================================================================

(rf/reg-event-fx
  :example/fancy-file-selector
  (fn [{:keys [db]} [_]]
    (let [;; In real implementation, fetch actual files
          sample-files ["README.md"
                        "package.json"
                        "src/core.cljs"
                        "src/commands.cljs"
                        "src/views.cljs"
                        "test/core_test.cljs"
                        "docs/API.md"
                        "docs/ARCHITECTURE.md"]]
      {:db (mb/push-frame db {:prompt "Select file (demo): "
                              :completions sample-files
                              :filtered-completions sample-files
                              :show-completions? true
                              :completion-index 0
                              :height-lines 15
                              :custom-renderer fancy-file-selector-renderer
                              :renderer-props {:file-info {:type "Demo"
                                                           :size "N/A"}}
                              :on-confirm [:example/fancy-file-selector-confirm]
                              :on-cancel [:minibuffer/deactivate]})})))

(rf/reg-event-fx
  :example/fancy-file-selector-confirm
  (fn [{:keys [db]} [_]]
    (let [input (mb/get-input db)
          frame (mb/current-frame db)
          selected-idx (:completion-index frame)
          candidates (:filtered-completions frame)
          selected-file (when (< selected-idx (count candidates))
                          (nth candidates selected-idx))]
      {:db (mb/pop-frame db)
       :fx [[:dispatch [:message (str "Selected: " (or selected-file input))]]]})))

;; Update filtered completions as user types
(rf/reg-event-db
  :example/fancy-file-selector-filter
  (fn [db [_ input]]
    (let [frame (mb/current-frame db)
          all-files (:completions frame)
          filtered (if (clojure.string/blank? input)
                     all-files
                     (filter #(clojure.string/includes?
                               (clojure.string/lower-case %)
                               (clojure.string/lower-case input))
                             all-files))]
      (mb/update-current-frame db {:filtered-completions filtered
                                   :completion-index 0}))))

;; Register command
(rf/dispatch-sync
  [:register-command
   :example-fancy-file-selector
   {:docstring "Demo: Custom minibuffer renderer with preview (Issue #46)"
    :interactive-spec nil
    :handler [:example/fancy-file-selector]}])

;; =============================================================================
;; Simpler Example: Custom Candidate Display
;; =============================================================================

(defn simple-custom-renderer
  "Simpler example: Just customize how candidates are displayed."
  [{:keys [minibuffer mode-line-style]}]
  (let [candidates (:filtered-completions minibuffer)
        selected-idx (:completion-index minibuffer)]
    [:div.simple-custom
     {:style {:padding "8px"}}
     [:div.candidates-title
      {:style {:font-weight "bold"
               :margin-bottom "8px"
               :color "#4EC9B0"}}
      (str "Showing " (count candidates) " matches:")]

     (for [[idx candidate] (map-indexed vector (take 10 candidates))]
       ^{:key idx}
       [:div.candidate
        {:style {:padding "4px 8px"
                 :margin "2px 0"
                 :background-color (when (= idx selected-idx)
                                     "rgba(100, 150, 255, 0.4)")
                 :border-radius "4px"
                 :cursor "pointer"}
         :on-click #(rf/dispatch [:minibuffer/select-completion idx])}
        [:span {:style {:color (when (= idx selected-idx) "#fff")}}
         candidate]])]))

(rf/reg-event-fx
  :example/simple-custom-minibuffer
  (fn [{:keys [db]} [_]]
    (let [sample-data ["Option Alpha"
                       "Option Beta"
                       "Option Gamma"
                       "Option Delta"
                       "Option Epsilon"]]
      {:db (mb/push-frame db {:prompt "Choose: "
                              :completions sample-data
                              :filtered-completions sample-data
                              :show-completions? true
                              :height-lines 8
                              :custom-renderer simple-custom-renderer
                              :on-confirm [:example/simple-confirm]
                              :on-cancel [:minibuffer/deactivate]})})))

(rf/reg-event-fx
  :example/simple-confirm
  (fn [{:keys [db]} [_]]
    (let [input (mb/get-input db)]
      {:db (mb/pop-frame db)
       :fx [[:dispatch [:message (str "You selected: " input)]]]})))

(rf/dispatch-sync
  [:register-command
   :example-simple-custom-minibuffer
   {:docstring "Demo: Simple custom candidate display"
    :interactive-spec nil
    :handler [:example/simple-custom-minibuffer]}])

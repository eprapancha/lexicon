(ns lexicon.effects
  "Re-frame effect handlers for DOM manipulation and side effects."
  (:require [re-frame.core :as rf]
            [lexicon.context :as ctx]
            [lexicon.log :as log]))

;; -- DOM Effect Handlers --

;; Inject or update a stylesheet in the document head.
;; Params:
;;   :id      - Unique ID for the <style> tag (updates existing if found)
;;   :content - CSS content to inject
;; Example:
;;   {:dom/inject-stylesheet {:id "lexicon-faces"
;;                            :content ".face-default { color: #000; }"}}
(rf/reg-fx
  :dom/inject-stylesheet
  (fn [{:keys [id content]}]
    (if-let [existing-style (js/document.getElementById id)]
      ;; Update existing stylesheet
      (do
        (log/info (str "🎨 Updating stylesheet: " id))
        (set! (.-textContent existing-style) content))
      ;; Create new stylesheet
      (let [style-element (js/document.createElement "style")]
        (log/info (str "🎨 Creating new stylesheet: " id))
        (set! (.-id style-element) id)
        (set! (.-type style-element) "text/css")
        (set! (.-textContent style-element) content)
        (.appendChild (.-head js/document) style-element)))))

;; Remove a stylesheet from the document head by ID.
;; Params: stylesheet-id (string)
;; Example:
;;   {:dom/remove-stylesheet "lexicon-faces"}
(rf/reg-fx
  :dom/remove-stylesheet
  (fn [stylesheet-id]
    (when-let [style-element (js/document.getElementById stylesheet-id)]
      (log/info (str "🗑️ Removing stylesheet: " stylesheet-id))
      (.remove style-element))))

;; Focus the minibuffer input element.
(rf/reg-fx
  :dom/focus-minibuffer
  (fn [_]
    (when-let [minibuffer-input (js/document.getElementById "minibuffer-input")]
      (.focus minibuffer-input))))

;; Focus the main editor element.
(rf/reg-fx
  :dom/focus-main-editor
  (fn [_]
    (when-let [editor (js/document.getElementById "editor-text")]
      (.focus editor))))

;; Set a CSS custom property (variable) on the document root.
;; Params:
;;   :name  - CSS variable name (e.g., "--lexicon-fg-default")
;;   :value - CSS value (e.g., "#000000")
;; Example:
;;   {:dom/set-css-variable {:name "--lexicon-fg-default"
;;                           :value "#000000"}}
(rf/reg-fx
  :dom/set-css-variable
  (fn [{:keys [name value]}]
    (.. js/document -documentElement -style (setProperty name value))))

;; Set multiple CSS custom properties at once.
;; Params: map of variable name -> value
;; Example:
;;   {:dom/set-css-variables {"--lexicon-fg-default" "#000"
;;                            "--lexicon-bg-default" "#fff"}}
(rf/reg-fx
  :dom/set-css-variables
  (fn [variables-map]
    (doseq [[name value] variables-map]
      (.. js/document -documentElement -style (setProperty name value)))))

;; -- Command Execution Context Effect --

;; Dispatch an event within a dynamic execution context.
;; Params:
;;   :event   - The event vector to dispatch
;;   :context - Map with :command, :buffer, :prefix-arg
;; Example:
;;   {:dispatch-with-context {:event [:insert-char "a"]
;;                            :context {:command :insert-char
;;                                      :buffer "buffer-1"
;;                                      :prefix-arg nil}}}
(rf/reg-fx
  :dispatch-with-context
  (fn [{:keys [event context]}]
    (ctx/with-command-context context
      #(rf/dispatch event))))

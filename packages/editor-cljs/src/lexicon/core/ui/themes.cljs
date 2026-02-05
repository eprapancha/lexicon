(ns lexicon.core.ui.themes
  "Theme system using CSS custom properties (variables).

  Themes define color schemes and font settings using CSS variables.
  The face system references these variables, so changing themes is instant
  (no stylesheet regeneration needed).

  Theme Structure:
  {:name \"My Theme\"
   :description \"A beautiful theme\"
   :kind :light  ; or :dark
   :palette {\"--lexicon-fg-default\" \"#000000\"
             \"--lexicon-bg-default\" \"#ffffff\"
             ...}}

  Themes are applied by updating :root CSS variables in one operation."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; -- CSS Variable Palette Definition --

;; Complete set of CSS variables used by the face system.
;; These are the "API" that themes must implement.

(def css-variable-palette
  "Complete palette of CSS variables that themes can customize."
  {;; -- Foreground Colors --
   :fg-default "--lexicon-fg-default"
   :fg-dim "--lexicon-fg-dim"
   :fg-intense "--lexicon-fg-intense"
   :fg-prompt "--lexicon-fg-prompt"
   :fg-link "--lexicon-fg-link"
   :fg-region "--lexicon-fg-region"
   :fg-mode-line "--lexicon-fg-mode-line"
   :fg-mode-line-inactive "--lexicon-fg-mode-line-inactive"

   ;; -- Background Colors --
   :bg-default "--lexicon-bg-default"
   :bg-dim "--lexicon-bg-dim"
   :bg-intense "--lexicon-bg-intense"
   :bg-region "--lexicon-bg-region"
   :bg-highlight "--lexicon-bg-highlight"
   :bg-mode-line "--lexicon-bg-mode-line"
   :bg-mode-line-inactive "--lexicon-bg-mode-line-inactive"
   :bg-minibuffer "--lexicon-bg-minibuffer"

   ;; -- Semantic Colors --
   :fg-error "--lexicon-fg-error"
   :bg-error "--lexicon-bg-error"
   :fg-warning "--lexicon-fg-warning"
   :bg-warning "--lexicon-bg-warning"
   :fg-success "--lexicon-fg-success"
   :bg-success "--lexicon-bg-success"
   :fg-info "--lexicon-fg-info"

   ;; -- Syntax Highlighting Colors --
   :fg-comment "--lexicon-fg-comment"
   :fg-doc "--lexicon-fg-doc"
   :fg-string "--lexicon-fg-string"
   :fg-keyword "--lexicon-fg-keyword"
   :fg-builtin "--lexicon-fg-builtin"
   :fg-function "--lexicon-fg-function"
   :fg-variable "--lexicon-fg-variable"
   :fg-type "--lexicon-fg-type"
   :fg-constant "--lexicon-fg-constant"
   :fg-operator "--lexicon-fg-operator"

   ;; -- Search & Matching --
   :bg-isearch "--lexicon-bg-isearch"
   :fg-isearch "--lexicon-fg-isearch"
   :bg-lazy-highlight "--lexicon-bg-lazy-highlight"

   ;; -- Completion --
   :bg-completion-selected "--lexicon-bg-completion-selected"
   :fg-completion-annotation "--lexicon-fg-completion-annotation"

   ;; -- Borders --
   :border-default "--lexicon-border-default"
   :border-mode-line "--lexicon-border-mode-line"
   :border-window "--lexicon-border-window"

   ;; -- Typography --
   :font-family-mono "--lexicon-font-family-mono"
   :font-family-sans "--lexicon-font-family-sans"
   :font-size "--lexicon-font-size"
   :line-height "--lexicon-line-height"})

;; -- Default Themes --

(def lexicon-base-light
  "Default light theme with WCAG AAA contrast compliance."
  {:name "Lexicon Base Light"
   :description "Clean, high-contrast light theme"
   :kind :light
   :palette {"--lexicon-fg-default" "#000000"
             "--lexicon-fg-dim" "#5f5f5f"
             "--lexicon-fg-intense" "#000000"
             "--lexicon-fg-prompt" "#0000ff"
             "--lexicon-fg-link" "#0000ee"
             "--lexicon-fg-region" "#000000"
             "--lexicon-fg-mode-line" "#000000"
             "--lexicon-fg-mode-line-inactive" "#5f5f5f"

             ;; Backgrounds
             "--lexicon-bg-default" "#ffffff"
             "--lexicon-bg-dim" "#f5f5f5"
             "--lexicon-bg-intense" "#e8e8e8"
             "--lexicon-bg-region" "#add8e6"
             "--lexicon-bg-highlight" "#ffffe0"
             "--lexicon-bg-mode-line" "#e5e5e5"
             "--lexicon-bg-mode-line-inactive" "#f0f0f0"
             "--lexicon-bg-minibuffer" "#f8f8f8"

             ;; Semantic colors
             "--lexicon-fg-error" "#cc0000"
             "--lexicon-bg-error" "#ffe0e0"
             "--lexicon-fg-warning" "#8b6914"
             "--lexicon-bg-warning" "#fff8dc"
             "--lexicon-fg-success" "#006400"
             "--lexicon-bg-success" "#e0ffe0"
             "--lexicon-fg-info" "#0000cd"

             ;; Syntax highlighting
             "--lexicon-fg-comment" "#b22222"
             "--lexicon-fg-doc" "#8b2252"
             "--lexicon-fg-string" "#008b00"
             "--lexicon-fg-keyword" "#a020f0"
             "--lexicon-fg-builtin" "#483d8b"
             "--lexicon-fg-function" "#0000ff"
             "--lexicon-fg-variable" "#a0522d"
             "--lexicon-fg-type" "#228b22"
             "--lexicon-fg-constant" "#008b8b"
             "--lexicon-fg-operator" "#000000"

             ;; Search
             "--lexicon-bg-isearch" "#ff69b4"
             "--lexicon-fg-isearch" "#ffffff"
             "--lexicon-bg-lazy-highlight" "#afeeee"

             ;; Completion
             "--lexicon-bg-completion-selected" "#add8e6"
             "--lexicon-fg-completion-annotation" "#5f5f5f"

             ;; Borders
             "--lexicon-border-default" "#cccccc"
             "--lexicon-border-mode-line" "#aaaaaa"
             "--lexicon-border-window" "#0000ff"

             ;; Typography
             "--lexicon-font-family-mono" "\"JetBrains Mono\", \"Fira Code\", \"Consolas\", \"Monaco\", monospace"
             "--lexicon-font-family-sans" "\"Inter\", \"SF Pro\", -apple-system, BlinkMacSystemFont, sans-serif"
             "--lexicon-font-size" "14px"
             "--lexicon-line-height" "1.5"

             ;; Hi-lock highlighting (#125)
             "--lexicon-bg-hi-yellow" "#ffff00"
             "--lexicon-bg-hi-pink" "#ffb6c1"
             "--lexicon-bg-hi-green" "#90ee90"
             "--lexicon-bg-hi-blue" "#b0e0e6"
             "--lexicon-bg-hi-black" "#c0c0c0"
             "--lexicon-bg-hi-red" "#ffcccc"
             "--lexicon-fg-hi-black" "#000000"}})

(def lexicon-base-dark
  "Default dark theme with WCAG AAA contrast compliance."
  {:name "Lexicon Base Dark"
   :description "Clean, high-contrast dark theme"
   :kind :dark
   :palette {"--lexicon-fg-default" "#ffffff"
             "--lexicon-fg-dim" "#b0b0b0"
             "--lexicon-fg-intense" "#ffffff"
             "--lexicon-fg-prompt" "#87ceeb"
             "--lexicon-fg-link" "#87cefa"
             "--lexicon-fg-region" "#ffffff"
             "--lexicon-fg-mode-line" "#ffffff"
             "--lexicon-fg-mode-line-inactive" "#b0b0b0"

             ;; Backgrounds
             "--lexicon-bg-default" "#1e1e1e"
             "--lexicon-bg-dim" "#2a2a2a"
             "--lexicon-bg-intense" "#3a3a3a"
             "--lexicon-bg-region" "#264f78"
             "--lexicon-bg-highlight" "#3a3a00"
             "--lexicon-bg-mode-line" "#2a2a2a"
             "--lexicon-bg-mode-line-inactive" "#252525"
             "--lexicon-bg-minibuffer" "#252525"

             ;; Semantic colors
             "--lexicon-fg-error" "#ff6b6b"
             "--lexicon-bg-error" "#4a0000"
             "--lexicon-fg-warning" "#ffd700"
             "--lexicon-bg-warning" "#4a3a00"
             "--lexicon-fg-success" "#90ee90"
             "--lexicon-bg-success" "#003a00"
             "--lexicon-fg-info" "#87ceeb"

             ;; Syntax highlighting (One Dark inspired)
             "--lexicon-fg-comment" "#7f848e"
             "--lexicon-fg-doc" "#9ca1aa"
             "--lexicon-fg-string" "#98c379"
             "--lexicon-fg-keyword" "#c678dd"
             "--lexicon-fg-builtin" "#e5c07b"
             "--lexicon-fg-function" "#61afef"
             "--lexicon-fg-variable" "#e06c75"
             "--lexicon-fg-type" "#56b6c2"
             "--lexicon-fg-constant" "#d19a66"
             "--lexicon-fg-operator" "#abb2bf"

             ;; Search
             "--lexicon-bg-isearch" "#ff1493"
             "--lexicon-fg-isearch" "#ffffff"
             "--lexicon-bg-lazy-highlight" "#2f4f4f"

             ;; Completion
             "--lexicon-bg-completion-selected" "#264f78"
             "--lexicon-fg-completion-annotation" "#7f848e"

             ;; Borders
             "--lexicon-border-default" "#3a3a3a"
             "--lexicon-border-mode-line" "#404040"
             "--lexicon-border-window" "#61afef"

             ;; Typography
             "--lexicon-font-family-mono" "\"JetBrains Mono\", \"Fira Code\", \"Consolas\", \"Monaco\", monospace"
             "--lexicon-font-family-sans" "\"Inter\", \"SF Pro\", -apple-system, BlinkMacSystemFont, sans-serif"
             "--lexicon-font-size" "14px"
             "--lexicon-line-height" "1.5"

             ;; Hi-lock highlighting (#125)
             "--lexicon-bg-hi-yellow" "#8b8b00"
             "--lexicon-bg-hi-pink" "#8b4557"
             "--lexicon-bg-hi-green" "#3a5f3a"
             "--lexicon-bg-hi-blue" "#3a5f8b"
             "--lexicon-bg-hi-black" "#4a4a4a"
             "--lexicon-bg-hi-red" "#5f3a3a"
             "--lexicon-fg-hi-black" "#ffffff"}})

;; -- Theme Registry --

(def default-themes
  "Built-in themes available by default."
  {:lexicon-base-light lexicon-base-light
   :lexicon-base-dark lexicon-base-dark})

;; -- Theme Application --

(defn theme->css
  "Convert theme palette to CSS :root variable definitions.
  Returns a CSS string like:
  :root {
    --lexicon-fg-default: #000000;
    --lexicon-bg-default: #ffffff;
    ...
  }"
  [theme]
  (let [palette (:palette theme)
        declarations (map (fn [[var-name value]]
                           (str "  " var-name ": " value ";"))
                         palette)]
    (str ":root {\n"
         (str/join "\n" declarations)
         "\n}")))

(defn inject-theme-css
  "Inject theme CSS variables into document :root.
  Updates or creates a <style> element with id 'lexicon-theme'."
  [theme]
  (let [css (theme->css theme)
        style-id "lexicon-theme"
        existing-style (js/document.getElementById style-id)]
    (if existing-style
      ;; Update existing style element
      (set! (.-textContent existing-style) css)
      ;; Create new style element
      (let [style-element (js/document.createElement "style")]
        (set! (.-id style-element) style-id)
        (set! (.-type style-element) "text/css")
        (set! (.-textContent style-element) css)
        (.appendChild (.-head js/document) style-element)))
    (println "✅ Theme applied:" (:name theme))))

;; -- Re-frame Integration --

;; Initialize theme system (load default theme)
(rf/reg-event-fx
  :theme/initialize
  (fn [{:keys [db]} _]
    (println "🎨 Initializing theme system...")
    (let [;; Default to light theme
          default-theme-id :lexicon-base-light
          default-theme (get default-themes default-theme-id)]
      ;; Inject theme CSS immediately
      (inject-theme-css default-theme)
      {:db (assoc db
                  :theme/registry default-themes
                  :theme/active default-theme-id
                  :theme/current default-theme)})))

;; Load a theme by ID
(rf/reg-event-fx
  :theme/load
  (fn [{:keys [db]} [_ theme-id]]
    (let [registry (:theme/registry db)
          theme (get registry theme-id)]
      (if theme
        (do
          (println "🎨 Loading theme:" (:name theme))
          (inject-theme-css theme)
          {:db (assoc db
                      :theme/active theme-id
                      :theme/current theme)
           :fx [[:dispatch [:echo/message (str "Loaded theme: " (:name theme))]]]})
        (do
          (println "⚠️ Unknown theme:" theme-id)
          {:fx [[:dispatch [:echo/message (str "Unknown theme: " theme-id)]]]})))  ))

;; Set a single CSS variable (for runtime customization)
(rf/reg-event-fx
  :theme/set-variable
  (fn [{:keys [db]} [_ var-name value]]
    (println "🎨 Setting CSS variable:" var-name "=" value)
    ;; Update the CSS variable directly on :root
    (.. js/document -documentElement -style (setProperty var-name value))
    {:db (assoc-in db [:theme/customizations var-name] value)
     :fx [[:dispatch [:echo/message (str "Set " var-name " = " value)]]]}))

;; Set font size (convenience command)
(rf/reg-event-fx
  :theme/set-font-size
  (fn [_ [_ size-px]]
    (let [size-str (str size-px "px")]
      {:fx [[:dispatch [:theme/set-variable "--lexicon-font-size" size-str]]]})))

;; -- Subscriptions --

;; Get current theme
(rf/reg-sub
  :theme/current
  (fn [db _]
    (:theme/current db)))

;; Get active theme ID
(rf/reg-sub
  :theme/active
  (fn [db _]
    (:theme/active db)))

;; Get all available themes
(rf/reg-sub
  :theme/available
  (fn [db _]
    (vals (:theme/registry db))))

;; -- Commands --

;; Register load-theme command
(defn register-commands! []
  (rf/dispatch [:register-command :load-theme
                {:description "Load a color theme"
                 :interactive {:type :completing-read
                              :prompt "Load theme: "
                              :collection [:lexicon-base-light :lexicon-base-dark]}
                 :handler (fn [theme-id-str]
                           (let [theme-id (keyword theme-id-str)]
                             (rf/dispatch [:theme/load theme-id])))}])

  (rf/dispatch [:register-command :set-font-size
                {:description "Set editor font size"
                 :interactive {:type :read-string
                              :prompt "Font size (px): "}
                 :handler (fn [size-str]
                           (let [size (js/parseInt size-str)]
                             (when-not (js/isNaN size)
                               (rf/dispatch [:theme/set-font-size size]))))}]))

;; Auto-register commands on namespace load
(register-commands!)

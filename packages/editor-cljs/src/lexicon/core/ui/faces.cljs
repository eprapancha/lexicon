(ns lexicon.core.ui.faces
  "Face system for visual styling using CSS custom properties (variables).

  Faces define visual attributes (foreground, background, font properties) that
  can be applied to text. Unlike static CSS, we use CSS variables to enable
  runtime theme switching without regenerating stylesheets.

  Architecture:
  - Face specs in db map attributes to CSS variable names
  - Static CSS classes reference variables: .face-default { color: var(--lexicon-fg-default); }
  - Themes swap variable values at :root level
  - No stylesheet regeneration needed for theme changes"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.log :as log]))

;; -- Face Attribute Conversion --

(defn- face-attr-to-css
  "Convert a single face attribute to CSS property-value pair.
  Returns vector of [property value] or nil if attribute not supported."
  [[attr value]]
  (case attr
    :foreground ["color" (str "var(" value ")")]
    :background ["background-color" (str "var(" value ")")]
    :font-family ["font-family" (str "var(" value ")")]
    :font-size ["font-size" (str "var(" value ")")]
    :font-weight ["font-weight" value]  ; bold, normal, etc. - literal values
    :font-slant (when (= value "italic") ["font-style" "italic"])
    :underline (cond
                 (boolean? value) (when value ["text-decoration" "underline"])
                 (string? value) ["text-decoration" (str "underline " (str "var(" value ")"))]
                 (map? value) ["text-decoration"
                               (str "underline "
                                    (:style value "solid")
                                    " "
                                    (str "var(" (:color value) ")"))])
    :box (when (map? value)
           ["border" (str (:line-width value 1) "px solid var(" (:color value) ")")])
    :extend nil  ; Extend is a semantic flag, not a CSS property
    :inherit nil  ; Inheritance handled separately
    nil))

(defn- face-spec-to-css-rules
  "Convert face spec to list of CSS property declarations.
  Example: {:foreground \"--lexicon-fg-default\" :font-weight \"bold\"}
  Returns: [\"color: var(--lexicon-fg-default);\" \"font-weight: bold;\"]"
  [face-spec]
  (->> face-spec
       (keep face-attr-to-css)
       (map (fn [[prop val]] (str "  " prop ": " val ";")))))

(defn- face-to-css-class
  "Generate CSS class for a face.
  Example: :default -> \".face-default { color: var(--lexicon-fg-default); }\""
  [face-name face-spec]
  (let [class-name (str "face-" (name face-name))
        rules (face-spec-to-css-rules face-spec)]
    (when (seq rules)
      (str "." class-name " {\n"
           (str/join "\n" rules)
           "\n}"))))

(defn generate-face-stylesheet
  "Generate complete CSS stylesheet from faces map.
  All faces use CSS variables, allowing runtime theme changes."
  [faces-map]
  (str "/* Lexicon Face System - Generated CSS */\n\n"
       (->> faces-map
            (map (fn [[name spec]] (face-to-css-class name spec)))
            (filter some?)
            (str/join "\n\n"))
       "\n"))

;; -- Face Inheritance --

(defn resolve-face-inheritance
  "Resolve face inheritance to get complete face spec.
  If face has :inherit attribute, merge parent face attributes."
  [faces-map face-name]
  (let [face-spec (get faces-map face-name)]
    (if-let [parent (:inherit face-spec)]
      (merge (resolve-face-inheritance faces-map parent)
             (dissoc face-spec :inherit))
      face-spec)))

;; -- Re-frame Integration --

(rf/reg-sub
  :faces/all
  (fn [db _]
    (:faces db)))

(rf/reg-sub
  :faces/get
  (fn [db [_ face-name]]
    (get-in db [:faces face-name])))

;; Get CSS class name for a face.
;; Example: (subscribe [:faces/class-for :default]) => "face-default"
(rf/reg-sub
  :faces/class-for
  (fn [db [_ face-name]]
    (str "face-" (name (or face-name :default)))))

;; Generate and inject face stylesheet on startup.
(rf/reg-event-fx
  :faces/initialize
  (fn [{:keys [db]} _]
    (let [faces (:faces db)
          stylesheet (generate-face-stylesheet faces)]
      (log/info (str "📋 Initializing face system with " (count faces) " faces"))
      {:fx [[:dom/inject-stylesheet {:id "lexicon-faces"
                                      :content stylesheet}]]})))

;; Define or update a face.
;; Usage: (dispatch [:faces/define :my-face {:foreground "--my-color"}])
(rf/reg-event-db
  :faces/define
  (fn [db [_ face-name face-spec]]
    (assoc-in db [:faces face-name] face-spec)))

;; Regenerate and update face stylesheet.
;; Call this after defining new faces.
(rf/reg-event-fx
  :faces/update-stylesheet
  (fn [{:keys [db]} _]
    (let [faces (:faces db)
          stylesheet (generate-face-stylesheet faces)]
      {:fx [[:dom/inject-stylesheet {:id "lexicon-faces"
                                      :content stylesheet}]]})))

;; -- Utility Functions --

(defn merge-face-specs
  "Merge multiple face specs with priority (later overrides earlier).
  Used for combining overlay faces, text properties, etc."
  [& face-specs]
  (apply merge face-specs))

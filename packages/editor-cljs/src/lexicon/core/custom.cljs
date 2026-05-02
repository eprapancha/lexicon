(ns lexicon.core.custom
  "Emacs-style customization system (defcustom, defgroup, custom-set-variables).

  This module owns:
  - Variable metadata registry (type, docstring, group, setter)
  - Group registry (hierarchy, members)
  - Persistence layer (localStorage)
  - CSS variable setter helpers

  Architecture:
  - defcustom declares a user-customizable variable with metadata
  - defgroup declares a customization group
  - custom-set-variables persists user overrides to localStorage
  - setopt is a convenience wrapper for custom-set-variables
  - Effective value = saved-value (if exists) > standard-value
  - Custom setters (:set) are called on value change for side effects"
  (:require [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [cljs.reader :as reader]))

;; =============================================================================
;; Registries
;; =============================================================================

;; Registry of defcustom variable metadata.
;; keyword -> {:standard-value any, :type keyword, :docstring string,
;;             :group keyword, :custom-set fn-or-nil}
(defonce variable-registry (atom {}))

;; Registry of customization groups.
;; keyword -> {:docstring string, :parent keyword-or-nil, :members #{keyword ...}}
(defonce group-registry (atom {}))

;; =============================================================================
;; CSS Variable Helpers
;; =============================================================================

(defn set-css-variable!
  "Set a CSS custom property on :root element."
  [var-name value]
  (.. js/document -documentElement -style (setProperty var-name value)))

(defn measure-char-width
  "Measure the width of a single monospace character at the current font settings.
  Uses a hidden canvas for fast, accurate measurement."
  []
  (try
    (let [canvas (js/document.createElement "canvas")
          ctx (.getContext canvas "2d")
          computed (js/getComputedStyle js/document.documentElement)
          font-size (.getPropertyValue computed "--lexicon-font-size")
          font-family (.getPropertyValue computed "--lexicon-font-family-mono")
          font-str (str (if (seq font-size) font-size "14px")
                        " "
                        (if (seq font-family) font-family "monospace"))]
      (set! (.-font ctx) font-str)
      (.-width (.measureText ctx "M")))
    (catch :default _e
      8.4)))

(defn update-font-metrics!
  "Recalculate and store font metrics in re-frame DB.
  Called when font-size or font-family changes.
  Uses direct swap! because this is called from custom setters which
  may be invoked from within event handlers (e.g., M-: eval)."
  []
  (let [char-width (measure-char-width)]
    (swap! rfdb/app-db assoc-in [:custom :char-width] char-width)))

(rf/reg-event-db
  :custom/set-font-metrics
  (fn [db [_ char-width]]
    (assoc-in db [:custom :char-width] char-width)))

(rf/reg-sub
  :custom/char-width
  (fn [db _]
    (get-in db [:custom :char-width] 8.4)))

;; =============================================================================
;; Persistence Layer (localStorage)
;; =============================================================================

(def ^:private storage-key "lexicon-custom-file")

(defn load-custom-file
  "Read saved customizations from localStorage.
  Returns map with :saved-values or empty map."
  []
  (try
    (when-let [raw (.getItem js/localStorage storage-key)]
      (let [data (reader/read-string raw)]
        (if (map? data)
          data
          (do (.warn js/console "lexicon-custom-file: not a map:" (pr-str data))
              nil))))
    (catch :default e
      (.warn js/console "lexicon-custom-file: read error:" (ex-message e))
      nil)))

(defn- save-custom-file
  "Write all saved-values to localStorage as EDN."
  [saved-values]
  (try
    (let [edn-str (pr-str {:saved-values saved-values :version 1})]
      (.setItem js/localStorage storage-key edn-str))
    (catch :default e
      (.warn js/console "Failed to save custom file:" (ex-message e)))))

;; =============================================================================
;; Custom Setter Lookup
;; =============================================================================

(defn get-custom-setter
  "Return the custom setter function for a variable, or nil."
  [var-kw]
  (get-in @variable-registry [var-kw :custom-set]))

(defn invoke-setter
  "Call a custom setter function with the new value."
  [setter value]
  (when (fn? setter)
    (try
      (setter value)
      (catch :default e
        (.warn js/console "Custom setter error:" (ex-message e))))))

;; =============================================================================
;; Re-frame Event: Load Saved Values
;; =============================================================================

(rf/reg-event-db
  :custom/load-saved-values
  (fn [db [_]]
    (let [saved (load-custom-file)]
      (-> db
          (assoc-in [:custom :saved-values] (:saved-values saved {}))
          (update :global-vars merge (:saved-values saved {}))))))

;; =============================================================================
;; Core API: defgroup
;; =============================================================================

(defn defgroup
  "Declare a customization group.

  Args:
    group-name - keyword naming the group
    docstring  - documentation string
    :parent    - parent group keyword (default :lexicon)

  Usage: (defgroup :lexicon-display \"Display settings\" :parent :lexicon)"
  [group-name docstring & {:keys [parent]}]
  (swap! group-registry assoc group-name
         {:docstring docstring
          :parent (or parent :lexicon)
          :members #{}})
  group-name)

;; =============================================================================
;; Core API: defcustom
;; =============================================================================

(defn defcustom
  "Declare a user-customizable variable with metadata.

  Resolves the effective value: saved > standard.
  Sets it in :global-vars and mirrors to lisp/global-vars atom.
  Calls custom setter if provided.

  Args:
    var-name       - keyword naming the variable
    standard-value - default value
    :type          - value type (:string, :integer, :boolean, :choice)
    :docstring     - documentation string
    :group         - group keyword
    :set           - custom setter fn (called with new value)

  Usage: (defcustom :font-size \"14px\"
           :type :string :docstring \"Font size\" :group :lexicon-display
           :set (fn [val] (set-css-variable! \"--lexicon-font-size\" val)))"
  [var-name standard-value & {:keys [type docstring group set]}]
  ;; 1. Register metadata
  (swap! variable-registry assoc var-name
         {:standard-value standard-value
          :type (or type :any)
          :docstring (or docstring "")
          :group group
          :custom-set set})

  ;; 2. Add to group's members set
  (when group
    (swap! group-registry update-in [group :members] (fnil conj #{}) var-name))

  ;; 3. Resolve effective value: saved > standard
  (let [db @rfdb/app-db
        saved-value (get-in db [:custom :saved-values var-name])
        effective-value (if (some? saved-value) saved-value standard-value)]

    ;; 4. Set effective value in :global-vars directly
    ;; Uses swap! because defcustom may be called from SCI eval within event handlers
    (swap! rfdb/app-db assoc-in [:global-vars var-name] effective-value)

    ;; 5. Call custom setter if provided
    (when set
      (invoke-setter set effective-value)))

  var-name)

;; =============================================================================
;; Core API: custom-set-variables
;; =============================================================================

(defn custom-set-variables
  "Persist user customizations.

  For each [var-name value] pair:
  1. Store in :custom :saved-values in re-frame DB
  2. Set in :global-vars
  3. Call custom setter if registered
  4. Persist all saved-values to localStorage

  Args: pairs of [var-name value] vectors

  Usage: (custom-set-variables [:font-size \"16px\"] [:fill-column 80])"
  [& pairs]
  ;; Uses direct swap! because custom-set-variables is often called from within
  ;; event handlers (e.g., M-: eval) where dispatch-sync is forbidden
  (let [kv-map (reduce (fn [acc [var-name value]]
                         (let [kw (if (keyword? var-name) var-name (keyword (name var-name)))]
                           ;; Update re-frame DB directly: saved-values and global-vars
                           (swap! rfdb/app-db
                                  (fn [db]
                                    (-> db
                                        (assoc-in [:custom :saved-values kw] value)
                                        (assoc-in [:global-vars kw] value))))
                           ;; Call custom setter
                           (when-let [setter (get-custom-setter kw)]
                             (invoke-setter setter value))
                           (assoc acc kw value)))
                       {} pairs)
        ;; Merge with any existing saved-values already in DB
        existing (get-in @rfdb/app-db [:custom :saved-values] {})
        all-saved (merge existing kv-map)]
    (save-custom-file all-saved))
  nil)

(rf/reg-event-db
  :custom/set-saved-value
  (fn [db [_ var-name value]]
    (assoc-in db [:custom :saved-values var-name] value)))

;; =============================================================================
;; Core API: setopt (convenience)
;; =============================================================================

(defn setopt
  "Set and persist user customizations (convenience wrapper).

  Usage: (setopt :font-size \"16px\" :fill-column 80)"
  [& args]
  (apply custom-set-variables (map vec (partition 2 args))))

;; =============================================================================
;; Initial Customization Groups
;; =============================================================================

(defgroup :lexicon "Lexicon editor" :parent nil)
(defgroup :lexicon-display "Display and appearance" :parent :lexicon)
(defgroup :lexicon-editing "Editing behavior" :parent :lexicon)
(defgroup :lexicon-packages "Package management" :parent :lexicon)

;; =============================================================================
;; register-defcustoms! — Called from main.cljs init
;; =============================================================================

(defn register-defcustoms!
  "Register all built-in defcustom variables.
  Called from main.cljs after :custom/load-saved-values."
  []
  ;; -- Editing variables --
  (defcustom :fill-column 70
    :type :integer
    :docstring "Column beyond which automatic line-wrapping should happen."
    :group :lexicon-editing)

  (defcustom :tab-width 8
    :type :integer
    :docstring "Distance between tab stops."
    :group :lexicon-editing)

  (defcustom :indent-tabs-mode nil
    :type :boolean
    :docstring "If non-nil, indentation uses tabs instead of spaces."
    :group :lexicon-editing)

  ;; -- Display variables (with CSS setters) --
  (defcustom :font-size "14px"
    :type :string
    :docstring "Editor font size in CSS units."
    :group :lexicon-display
    :set (fn [val]
           (set-css-variable! "--lexicon-font-size" val)
           (update-font-metrics!)))

  (defcustom :font-family
    "\"JetBrains Mono\", \"Fira Code\", \"Consolas\", \"Monaco\", monospace"
    :type :string
    :docstring "Editor monospace font family."
    :group :lexicon-display
    :set (fn [val]
           (set-css-variable! "--lexicon-font-family-mono" val)
           (update-font-metrics!)))

  (defcustom :line-height "1.5"
    :type :string
    :docstring "Editor line height."
    :group :lexicon-display
    :set (fn [val] (set-css-variable! "--lexicon-line-height" val)))

  (defcustom :custom-enabled-themes [:lexicon-base-dark]
    :type :choice
    :docstring "List of enabled custom themes."
    :group :lexicon-display
    :set (fn [val]
           ;; Only dispatch theme load if theme registry is initialized
           ;; (avoids error during startup when register-defcustoms! runs
           ;; before :theme/initialize)
           ;; Uses async dispatch because theme/load has side effects (inject CSS)
           ;; that are safe to run asynchronously
           (when (:theme/registry @rfdb/app-db)
             (rf/dispatch [:theme/load (first val)]))))

  ;; -- Package management --
  (defcustom :package-archives "https://eprapancha.github.io/lexpkgs/packages"
    :type :string
    :docstring "Base URL of the package archive server."
    :group :lexicon-packages))

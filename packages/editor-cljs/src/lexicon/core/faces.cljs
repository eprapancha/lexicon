(ns lexicon.core.faces
  "Emacs-compatible face (theming) system.

  Faces define visual attributes (colors, fonts, etc.) for UI elements.
  Themes provide color palettes that faces reference.

  This implementation targets Modus themes (default in Emacs 28+).")

;; -- Color Palettes --

(def modus-operandi-palette
  "Modus Operandi (light theme) color palette.

  High-contrast light theme conforming to WCAG AAA standards.
  Created by Protesilaos Stavrou."
  {:name "Modus Operandi"
   :type :light

   ;; Base colors
   :bg-main "#ffffff"
   :bg-dim "#f2f2f2"
   :fg-main "#000000"
   :fg-dim "#595959"
   :fg-alt "#505050"

   ;; Mode-line
   :bg-mode-line-active "#c8c8c8"
   :fg-mode-line-active "#000000"
   :border-mode-line-active "#959595"

   :bg-mode-line-inactive "#e6e6e6"
   :fg-mode-line-inactive "#585858"
   :border-mode-line-inactive "#c4c4c4"

   ;; Minibuffer
   :bg-prompt "#f0f0f0"
   :fg-prompt "#0031a9"  ; Blue for prompts

   ;; Syntax highlighting (basic set)
   :keyword "#5317ac"     ; Purple
   :string "#2a5045"      ; Green
   :comment "#505050"     ; Gray
   :number "#0000c0"      ; Blue
   :function "#721045"    ; Magenta
   :variable "#00538b"    ; Cyan

   ;; Special highlights
   :region-bg "#bcbcbc"
   :cursor "#000000"
   :hl-line "#e0e0e0"})

(def modus-vivendi-palette
  "Modus Vivendi (dark theme) color palette.

  High-contrast dark theme conforming to WCAG AAA standards.
  Created by Protesilaos Stavrou."
  {:name "Modus Vivendi"
   :type :dark

   ;; Base colors
   :bg-main "#000000"
   :bg-dim "#1e1e1e"
   :fg-main "#ffffff"
   :fg-dim "#989898"
   :fg-alt "#a8a8a8"

   ;; Mode-line
   :bg-mode-line-active "#505050"
   :fg-mode-line-active "#ffffff"
   :border-mode-line-active "#959595"

   :bg-mode-line-inactive "#2d2d2d"
   :fg-mode-line-inactive "#969696"
   :border-mode-line-inactive "#606060"

   ;; Minibuffer
   :bg-prompt "#1e1e1e"
   :fg-prompt "#00d3d0"  ; Cyan for prompts

   ;; Syntax highlighting (basic set)
   :keyword "#b6a0ff"     ; Purple
   :string "#44bc44"      ; Green
   :comment "#989898"     ; Gray
   :number "#00bcff"      ; Blue
   :function "#feacd0"    ; Magenta
   :variable "#00d3d0"    ; Cyan

   ;; Special highlights
   :region-bg "#3c3c3c"
   :cursor "#ffffff"
   :hl-line "#2a2a2a"})

;; -- Face Registry --

(def default-faces
  "Default face definitions.

  Each face maps to theme palette keys for different attributes.
  Faces can inherit from other faces using :inherit key."
  {:default
   {:foreground :fg-main
    :background :bg-main}

   :mode-line
   {:foreground :fg-mode-line-active
    :background :bg-mode-line-active
    :border :border-mode-line-active
    :box {:line-width 1
          :color :border-mode-line-active
          :style :released-button}}

   :mode-line-inactive
   {:foreground :fg-mode-line-inactive
    :background :bg-mode-line-inactive
    :border :border-mode-line-inactive
    :box {:line-width 1
          :color :border-mode-line-inactive
          :style :released-button}}

   :mode-line-buffer-id
   {:inherit :mode-line
    :weight :bold}

   :minibuffer-prompt
   {:foreground :fg-prompt
    :weight :bold}

   :region
   {:background :region-bg}

   :cursor
   {:background :cursor}

   :hl-line
   {:background :hl-line}

   ;; Syntax highlighting faces
   :font-lock-keyword-face
   {:foreground :keyword
    :weight :bold}

   :font-lock-string-face
   {:foreground :string}

   :font-lock-comment-face
   {:foreground :comment
    :slant :italic}

   :font-lock-number-face
   {:foreground :number}

   :font-lock-function-name-face
   {:foreground :function
    :weight :bold}

   :font-lock-variable-name-face
   {:foreground :variable}})

;; -- Theme Registry --

(def themes
  "Available themes registry."
  {:modus-operandi modus-operandi-palette
   :modus-vivendi modus-vivendi-palette})

;; -- Face Resolution Functions --

(defn resolve-palette-color
  "Resolve a palette key to its hex color value.

  Args:
    palette - Theme palette map
    key     - Keyword for color (e.g., :fg-main, :bg-mode-line-active)

  Returns:
    Hex color string (e.g., '#ffffff') or nil if not found."
  [palette key]
  (when (keyword? key)
    (get palette key)))

(defn resolve-face-attribute
  "Resolve a face attribute to its final value.

  Handles inheritance and palette lookups.

  Args:
    faces   - Face registry map
    palette - Theme palette map
    face    - Face keyword (e.g., :mode-line)
    attr    - Attribute keyword (e.g., :foreground, :background)

  Returns:
    Resolved value (hex color for colors, or raw value for other attrs)."
  [faces palette face attr]
  (let [face-def (get faces face)]
    (if-let [value (get face-def attr)]
      ;; If it's a keyword, resolve from palette
      (if (keyword? value)
        (resolve-palette-color palette value)
        value)
      ;; Check for inheritance
      (when-let [inherited (get face-def :inherit)]
        (resolve-face-attribute faces palette inherited attr)))))

(defn get-face-foreground
  "Get foreground color for a face.

  Args:
    theme - Theme keyword (e.g., :modus-vivendi)
    face  - Face keyword (e.g., :mode-line)

  Returns:
    Hex color string or nil."
  [theme face]
  (let [palette (get themes theme)
        faces default-faces]
    (resolve-face-attribute faces palette face :foreground)))

(defn get-face-background
  "Get background color for a face.

  Args:
    theme - Theme keyword (e.g., :modus-vivendi)
    face  - Face keyword (e.g., :mode-line)

  Returns:
    Hex color string or nil."
  [theme face]
  (let [palette (get themes theme)
        faces default-faces]
    (resolve-face-attribute faces palette face :background)))

(defn get-face-border
  "Get border color for a face.

  Args:
    theme - Theme keyword (e.g., :modus-vivendi)
    face  - Face keyword (e.g., :mode-line)

  Returns:
    Hex color string or nil."
  [theme face]
  (let [palette (get themes theme)
        faces default-faces]
    (resolve-face-attribute faces palette face :border)))

(defn get-face-box
  "Get box (3D border) specification for a face.

  Returns a map with :line-width, :color, :style.
  The :color will be resolved from palette.

  Args:
    theme - Theme keyword (e.g., :modus-vivendi)
    face  - Face keyword (e.g., :mode-line)

  Returns:
    Map with box attributes or nil."
  [theme face]
  (let [palette (get themes theme)
        faces default-faces
        box (resolve-face-attribute faces palette face :box)]
    (when box
      ;; Resolve :color if it's a palette key
      (if-let [color-key (:color box)]
        (assoc box :color (resolve-palette-color palette color-key))
        box))))

(defn face-to-style
  "Convert a face to a CSS style map.

  This is the main function views will use to get styling.

  Args:
    theme - Theme keyword (e.g., :modus-vivendi)
    face  - Face keyword (e.g., :mode-line)

  Returns:
    Map suitable for Reagent :style attribute."
  [theme face]
  (let [fg (get-face-foreground theme face)
        bg (get-face-background theme face)
        border (get-face-border theme face)
        box (get-face-box theme face)]
    (cond-> {}
      fg (assoc :color fg)
      bg (assoc :background-color bg)
      border (assoc :border-color border)

      ;; Convert Emacs box to CSS box-shadow for 3D effect
      box (assoc :box-shadow
                 (let [{:keys [line-width color style]} box]
                   (case style
                     :released-button
                     (str "inset 0 " (- line-width) "px 0 0 " color)

                     ;; Default flat border
                     (str "0 0 0 " line-width "px " color)))))))

;; -- Helper Functions for Mode-line Elements --

(defn get-modified-indicator
  "Get mode-line modified status indicator.

  Emacs convention:
    ** - Modified
    -- - Unmodified
    %% - Read-only
    %* - Read-only and modified

  Args:
    modified? - Boolean, is buffer modified
    read-only? - Boolean, is buffer read-only

  Returns:
    String indicator (2 characters)."
  [modified? read-only?]
  (cond
    (and read-only? modified?) "%*"
    read-only? "%%"
    modified? "**"
    :else "--"))

(defn get-encoding-indicator
  "Get mode-line encoding indicator.

  Emacs convention:
    U: - UTF-8 with Unix line endings
    U\\ - UTF-8 with DOS line endings
    U/ - UTF-8 with Mac line endings
    1: - Latin-1
    etc.

  For now, we assume UTF-8 Unix.

  Returns:
    String indicator."
  []
  "U:")

(defn get-position-indicator
  "Get mode-line position indicator.

  Emacs convention:
    Top - At beginning
    Bot - At end
    All - Buffer fits in window
    45% - Percentage through buffer

  Args:
    cursor-line - Current line number (0-based)
    total-lines - Total lines in buffer

  Returns:
    String indicator."
  [cursor-line total-lines]
  (cond
    (<= total-lines 1) "All"
    (= cursor-line 0) "Top"
    (>= cursor-line (dec total-lines)) "Bot"
    :else (str (int (* 100 (/ (inc cursor-line) total-lines))) "%")))

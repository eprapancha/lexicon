(ns lexicon.core.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [lexicon.core.constants :as const]
            [lexicon.core.subs :as subs]
            [lexicon.core.events :as events]
            [lexicon.core.log :as log]
            [lexicon.core.modes.hl-line]
            [lexicon.core.modes.show-paren]
            [lexicon.core.modes.whitespace :as whitespace]
            [lexicon.core.modes.display-line-numbers]
            [lexicon.core.faces :as faces]
            [lexicon.core.ui.mode-line :as mode-line]))

;; -- Input Event Handling --

(defn handle-keydown
  "Handle keydown events - the core of our new Emacs-style input system"
  [event]
  (let [ctrl? (.-ctrlKey event)
        key (.-key event)]
    ;; Prevent dangerous browser shortcuts early and stop propagation
    (when (and ctrl? (or (= key "w") (= key "t") (= key "n")))
      (.preventDefault event)
      (.stopPropagation event))

    (let [key-str (events/key-event-to-string event)]
      (when key-str
        ;; Prevent default browser behavior for bound keys
        (.preventDefault event)
        (.stopPropagation event)
        ;; Dispatch to our new key sequence handler
        (rf/dispatch [:handle-key-sequence key-str])))))

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
        cursor-pos @(rf/subscribe [:lexicon.core.subs/cursor-position])]
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
             (reset! input-ref-atom element)
             ;; Add keydown listener in capture phase to intercept browser shortcuts
             (.addEventListener element "keydown" handle-keydown #js {:capture true})
             (.focus element)))
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

;; =============================================================================
;; Font-Lock Rendering (Issue #130)
;; =============================================================================

(defn- get-face-color
  "Get the foreground color for a face from the current theme."
  [theme face-keyword]
  (faces/get-face-foreground theme face-keyword))

(defn apply-font-lock-to-line
  "Apply font-lock face colors to a line of text.

   Args:
     line-text - The text of the line
     line-start-pos - The absolute character position where this line starts
     face-intervals - Vector of {:start :end :value face-keyword} intervals
     theme - Current theme keyword (e.g., :modus-vivendi)
     default-color - Default text color

   Returns:
     Hiccup vector with colored spans."
  [line-text line-start-pos face-intervals theme default-color]
  (let [line-end-pos (+ line-start-pos (count line-text))
        ;; Find intervals that overlap with this line
        overlapping (filter (fn [{:keys [start end]}]
                              (and (< start line-end-pos)
                                   (> end line-start-pos)))
                            face-intervals)]
    (if (empty? overlapping)
      ;; No face intervals, return plain text with default color
      [:span.line-content {:style {:color default-color}} line-text]
      ;; Build colored spans
      (let [;; Create change points within this line
            changes (atom (sorted-set 0 (count line-text)))
            _ (doseq [{:keys [start end]} overlapping]
                (let [rel-start (max 0 (- start line-start-pos))
                      rel-end (min (count line-text) (- end line-start-pos))]
                  (when (< rel-start (count line-text))
                    (swap! changes conj rel-start))
                  (when (and (> rel-end 0) (<= rel-end (count line-text)))
                    (swap! changes conj rel-end))))
            positions (vec @changes)
            ;; Build spans
            spans (atom [])]
        (doseq [i (range (dec (count positions)))]
          (let [span-start (nth positions i)
                span-end (nth positions (inc i))
                span-text (subs line-text span-start span-end)
                abs-pos (+ line-start-pos span-start)
                ;; Find the face at this position
                face (some (fn [{:keys [start end value]}]
                             (when (and (<= start abs-pos) (< abs-pos end))
                               value))
                           overlapping)
                color (if face
                        (or (get-face-color theme face) default-color)
                        default-color)
                ;; Get additional face attributes (bold, italic)
                face-def (when face (get faces/default-faces face))
                weight (when (= (:weight face-def) :bold) "bold")
                slant (when (= (:slant face-def) :italic) "italic")]
            (when (seq span-text)
              (swap! spans conj
                     [:span {:style (cond-> {:color color}
                                      weight (assoc :font-weight weight)
                                      slant (assoc :font-style slant))}
                      span-text]))))
        (into [:span.line-content] @spans)))))

(defn render-line-with-font-lock
  "Render a line with font-lock highlighting, falling back to decorations.

   This is the main entry point for line rendering with syntax highlighting.
   It combines font-lock faces from text properties with decoration-based
   highlighting (diagnostics, region, paren matching, etc.)."
  [line-text line-number line-start-pos face-intervals theme decorations default-color]
  (let [;; First apply font-lock faces
        font-locked (apply-font-lock-to-line line-text line-start-pos face-intervals theme default-color)
        ;; Check for line-level decorations (diagnostics, etc.)
        line-decorations (filter #(= (:line (:from %)) line-number) decorations)
        line-diagnostics (filter #(= (:type %) :diagnostic) line-decorations)]
    (if (empty? line-decorations)
      ;; No decorations, just return font-locked content
      font-locked
      ;; Apply decorations on top of font-locked content
      ;; For now, decorations take priority over font-lock for their spans
      (apply-decorations-to-line line-text line-number decorations))))

(defn linear-to-line-col
  "Convert linear position to line/column coordinates"
  [text linear-pos]
  (let [lines (clojure.string/split text #"\n" -1)
        result (loop [remaining-pos linear-pos
                      current-line 0]
                 (if (>= current-line (count lines))
                   {:line (dec current-line) :column 0}
                   (let [line-length (count (nth lines current-line))
                         line-length-with-newline (inc line-length)]
                     (if (< remaining-pos line-length-with-newline)
                       {:line current-line :column remaining-pos}
                       (recur (- remaining-pos line-length-with-newline)
                              (inc current-line))))))]
    result))

(defn create-region-decorations
  "Create decorations for the active region (mark to cursor)"
  [mark-position cursor-pos text]
  (when (and mark-position cursor-pos text)
    (let [cursor-linear (let [{:keys [line column]} cursor-pos
                             lines (clojure.string/split text #"\n" -1)
                             lines-before (take line lines)
                             chars-before (reduce + (map #(inc (count %)) lines-before))]
                         (+ chars-before column))
          [start-pos end-pos] (if (< mark-position cursor-linear)
                                [mark-position cursor-linear]
                                [cursor-linear mark-position])
          start-coords (linear-to-line-col text start-pos)
          end-coords (linear-to-line-col text end-pos)
          start-line (:line start-coords)
          end-line (:line end-coords)]

      ;; Create decorations for each line in the region
      (for [line-num (range start-line (inc end-line))]
        (let [start-col (if (= line-num start-line) (:column start-coords) 0)
              line-text (nth (clojure.string/split text #"\n" -1) line-num "")
              end-col (if (= line-num end-line)
                        (:column end-coords)
                        (count line-text))]
          {:from {:line line-num :column start-col}
           :to {:line line-num :column end-col}
           :class "region-highlight"
           :type :region})))))

(defn create-paren-decorations
  "Create decorations for matching parentheses (show-paren-mode).
  Takes match-info {:paren-pos :match-pos :matched?} and buffer text."
  [match-info text]
  (when (and match-info text (:matched? match-info))
    (let [{:keys [paren-pos match-pos]} match-info
          paren-coords (linear-to-line-col text paren-pos)
          match-coords (linear-to-line-col text match-pos)]
      ;; Create decorations for both the paren at cursor and the matching paren
      [{:from {:line (:line paren-coords) :column (:column paren-coords)}
        :to {:line (:line paren-coords) :column (inc (:column paren-coords))}
        :class "show-paren-match"
        :type :paren}
       {:from {:line (:line match-coords) :column (:column match-coords)}
        :to {:line (:line match-coords) :column (inc (:column match-coords))}
        :class "show-paren-match"
        :type :paren}])))

(defn editor-gutter
  "IDE-style gutter with line numbers and diagnostic markers"
  []
  (let [visible-lines @(rf/subscribe [:lexicon.core.subs/visible-lines])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [:lexicon.core.subs/viewport])
        decorations @(rf/subscribe [:lexicon.core.subs/all-decorations])
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
  (let [visible-lines @(rf/subscribe [:lexicon.core.subs/visible-lines])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [:lexicon.core.subs/viewport])
        base-decorations @(rf/subscribe [:lexicon.core.subs/all-decorations])
        cursor-pos @(rf/subscribe [:lexicon.core.subs/cursor-position])
        mark-position @(rf/subscribe [:mark-position])
        region-active? @(rf/subscribe [:region-active?])
        buffer-content @(rf/subscribe [:buffer-content])
        hl-line-enabled? @(rf/subscribe [:hl-line/enabled?])  ; Issue #123: hl-line-mode
        ;; DEBUG: Log what we're getting
        ;; Auto-scroll: Ensure cursor stays visible in viewport
        _ (when (and cursor-pos viewport)
            (let [{:keys [line]} cursor-pos
                  start-line (:start-line viewport)
                  end-line (:end-line viewport)
                  visible-height (when (and start-line end-line)
                                   (- end-line start-line))]
              (when visible-height
                (println "🔍 Auto-scroll check - cursor line:" line "viewport:" start-line "-" end-line "height:" visible-height)
                ;; Scroll if cursor is above or below visible area
                (cond
                  ;; Cursor above viewport - scroll up to make it visible
                  (< line start-line)
                  (do
                    (println "📜 Scrolling UP - cursor above viewport")
                    (rf/dispatch [:update-viewport line (+ line visible-height)]))

                  ;; Cursor below viewport - scroll down to make it visible
                  (>= line end-line)
                  (let [new-end (+ line 1)  ; +1 to show line below cursor
                        new-start (max 0 (- new-end visible-height))]
                    (println "📜 Scrolling DOWN - cursor below viewport. New viewport:" new-start "-" new-end)
                    (rf/dispatch [:update-viewport new-start new-end]))))))
        ;; Add region decorations if region is active
        region-decorations (when region-active?
                            (create-region-decorations mark-position cursor-pos buffer-content))
        ;; Add paren match decorations if show-paren-mode is enabled
        paren-match-info @(rf/subscribe [:show-paren/match-info])
        paren-decorations (when paren-match-info
                           (create-paren-decorations paren-match-info buffer-content))
        decorations (cond-> base-decorations
                      region-decorations (concat region-decorations)
                      paren-decorations (concat paren-decorations))
        handle-click (fn [e]
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
                         (rf/dispatch [:click-to-position absolute-line clicked-column])))]

    [:div.text-container
     {:style {:flex "1"
              :position "relative"
              :overflow "hidden"
              :min-height "100%"}  ; Ensure container fills height
      :on-click handle-click}

     [:div.editable-area
      {:on-click (fn [e]
                   (when-let [hidden-input @hidden-input-ref]
                     (.focus hidden-input)))
       :style {:background-color "rgba(37, 37, 38, 0.5)"  ; Semi-transparent highlight
               :border-radius "4px"
               :position "absolute"  ; Changed from relative
               :top "0"
               :left "0"
               :right "0"
               :bottom "0"
               :padding "20px 8px"  ; Top/bottom 20px, left/right 8px
               :box-sizing "border-box"  ; Include padding in width calculation
               :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
               :font-size "14px"
               :line-height (str line-height "px")
               :color "#d4d4d4"
               :white-space "pre-wrap"
               :cursor "text"  ; Show text cursor
               :pointer-events "auto"  ; Enable clicks
               :z-index "1"}}  ; Ensure it's above other elements

      ;; Render visible lines as individual divs with syntax highlighting
      (when visible-lines
        (let [lines (clojure.string/split visible-lines #"\n" -1)  ; -1 keeps trailing empty strings
              current-line (when cursor-pos (:line cursor-pos))]
          (for [[idx line] (map-indexed vector lines)]
            (let [line-number (+ (:start-line viewport 0) idx)
                  is-current-line? (and hl-line-enabled? (= line-number current-line))]
              ^{:key line-number}
              [:div.text-line
               {:style (cond-> {:min-height (str line-height "px")
                                :color "#d4d4d4"}
                         ;; Add hl-line background when enabled and on current line
                         is-current-line? (assoc :background-color "rgba(80, 80, 120, 0.6)"))}
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


(defn window-pane-gutter
  "Gutter for a specific window"
  [window-id]
  (let [show-line-numbers? @(rf/subscribe [:display-line-numbers/enabled? window-id])
        visible-lines @(rf/subscribe [:lexicon.core.subs/window-visible-lines window-id])
        line-height @(rf/subscribe [:line-height])
        viewport @(rf/subscribe [:lexicon.core.subs/window-viewport window-id])
        decorations @(rf/subscribe [:lexicon.core.subs/window-decorations window-id])
        diagnostic-decorations (filter #(= (:type %) :diagnostic) decorations)]

    ;; Only render gutter when line numbers are enabled
    (when show-line-numbers?
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

     (when visible-lines
       (let [lines (clojure.string/split visible-lines #"\n" -1)
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
           lines)))])))

(defn window-pane-text
  "Text rendering pane for a specific window"
  [window-id is-active? hidden-input-ref]
  (r/with-let [container-ref (atom nil)
               visible-lines-atom (atom nil)]
    (let [visible-lines @(rf/subscribe [:lexicon.core.subs/window-visible-lines window-id])
          line-height @(rf/subscribe [:line-height])
          viewport @(rf/subscribe [:lexicon.core.subs/window-viewport window-id])
          base-decorations @(rf/subscribe [:lexicon.core.subs/window-decorations window-id])
          cursor-pos @(rf/subscribe [:lexicon.core.subs/window-cursor-position window-id])
          mark-position @(rf/subscribe [:lexicon.core.subs/window-mark-position window-id])
          buffer-content @(rf/subscribe [:lexicon.core.subs/window-buffer-content window-id])
          buffer-name @(rf/subscribe [:lexicon.core.subs/window-buffer-name window-id])  ; Issue #137
          cursor-owner @(rf/subscribe [:cursor-owner])  ; Issue #62: Cursor singleton
          owns-cursor? (= cursor-owner window-id)       ; Only render cursor if THIS window owns it
          hl-line-enabled? @(rf/subscribe [:hl-line/enabled?])  ; Issue #123: hl-line-mode
          paren-match-info @(rf/subscribe [:show-paren/match-info])  ; Issue #123: show-paren-mode
          whitespace-enabled? @(rf/subscribe [:whitespace/enabled?])  ; Issue #123: whitespace-mode
          is-completions-buffer? (= buffer-name "*Completions*")  ; Issue #137: Clickable completions
          ;; Issue #138: For *Completions*, get span-based cursor highlight instead of hl-line
          current-entry @(rf/subscribe [:completion-help/current-entry])
          ;; Issue #130: Font-lock syntax highlighting
          face-intervals @(rf/subscribe [:lexicon.core.subs/window-face-intervals window-id])
          font-lock-enabled? @(rf/subscribe [:lexicon.core.subs/window-font-lock-enabled? window-id])
          current-theme @(rf/subscribe [:current-theme])
          region-active? (not (nil? mark-position))
          region-decorations (when region-active?
                              (create-region-decorations mark-position cursor-pos buffer-content))
          paren-decorations (when paren-match-info
                              (create-paren-decorations paren-match-info buffer-content))
          decorations (cond-> base-decorations
                        region-decorations (concat region-decorations)
                        paren-decorations (concat paren-decorations))
          ;; Calculate actual visible lines based on container height
          _ (when @container-ref
              (let [container @container-ref
                    height (.-clientHeight container)
                    padding-top 20
                    padding-bottom 20
                    usable-height (- height padding-top padding-bottom)
                    new-visible-lines (max 1 (int (/ usable-height line-height)))]
                (when (not= @visible-lines-atom new-visible-lines)
                  (reset! visible-lines-atom new-visible-lines))))
          actual-visible-lines @visible-lines-atom
          ;; Auto-scroll: Ensure cursor stays visible in viewport
          _ (when (and cursor-pos viewport is-active? actual-visible-lines)
              (let [{:keys [line]} cursor-pos
                    start-line (:start-line viewport)
                    end-line (:end-line viewport)
                    current-visible-height (- end-line start-line)]
                ;; Update viewport size if it doesn't match actual visible lines
                (when (not= current-visible-height actual-visible-lines)
                  (rf/dispatch [:update-viewport start-line (+ start-line actual-visible-lines)]))
                ;; Scroll if cursor is above or below visible area
                (cond
                  ;; Cursor above viewport - scroll up to make it visible
                  (< line start-line)
                  (rf/dispatch [:update-viewport line (+ line actual-visible-lines)])

                  ;; Cursor below viewport - scroll down to make it visible
                  (>= line end-line)
                  (let [new-end (+ line 1)
                        new-start (max 0 (- new-end actual-visible-lines))]
                    (rf/dispatch [:update-viewport new-start new-end])))))
        handle-click (fn [e]
                       (.stopPropagation e)
                       (let [rect (-> e .-currentTarget .getBoundingClientRect)
                             click-x (- (.-clientX e) (.-left rect))
                             click-y (- (.-clientY e) (.-top rect))
                             char-width 8.4
                             left-padding 8
                             top-padding 20
                             clicked-line (int (/ (- click-y top-padding) line-height))
                             absolute-line (+ clicked-line (:start-line viewport 0))]
                         (if is-completions-buffer?
                           ;; Issue #137: Click in *Completions* buffer selects completion
                           ;; Calculate linear position and use text properties
                           (let [clicked-column (max 0 (int (/ (- click-x left-padding) char-width)))
                                 lines (clojure.string/split (or buffer-content "") #"\n" -1)
                                 chars-before (reduce + (map #(inc (count %)) (take absolute-line lines)))
                                 linear-pos (+ chars-before clicked-column)]
                             (rf/dispatch [:completion-list/click-position linear-pos]))
                           ;; Normal buffer click - set position and focus
                           (do
                             (rf/dispatch [:set-active-window window-id])
                             (when-let [hidden-input @hidden-input-ref]
                               (.focus hidden-input))
                             (let [clicked-column (max 0 (int (/ (- click-x left-padding) char-width)))]
                               (rf/dispatch [:click-to-position absolute-line clicked-column]))))))]

    [:div.text-container
     {:ref (fn [el] (reset! container-ref el))
      :style {:flex "1"
              :position "relative"
              :overflow "hidden"
              :min-height "100%"}
      :on-click handle-click}

     [:div.editable-area
      {:on-click (fn [e]
                   (when-let [hidden-input @hidden-input-ref]
                     (.focus hidden-input)))
       :style {:background-color "rgba(37, 37, 38, 0.5)"
               :border-radius "4px"
               :position "absolute"
               :top "0"
               :left "0"
               :right "0"
               :bottom "0"
               :padding "20px 8px"
               :box-sizing "border-box"
               :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
               :font-size "14px"
               :line-height (str line-height "px")
               :color "#d4d4d4"
               :white-space "pre-wrap"
               :cursor "text"
               :pointer-events "auto"
               :z-index "1"}}

      ;; Render visible lines
      (when visible-lines
        (let [lines (clojure.string/split visible-lines #"\n" -1)
              all-buffer-lines (when buffer-content (clojure.string/split buffer-content #"\n" -1))
              current-line (when cursor-pos (:line cursor-pos))
              ;; Issue #137: Ensure cursor line is in visible viewport to prevent ghost highlights
              viewport-start (:start-line viewport 0)
              viewport-end (:end-line viewport 1000)
              cursor-in-viewport? (and current-line
                                       (>= current-line viewport-start)
                                       (<= current-line viewport-end))
              ;; Issue #130: Calculate line start positions for font-lock
              ;; Pre-compute cumulative character positions for each line
              line-start-positions (when (and font-lock-enabled? (seq face-intervals))
                                     (loop [positions [0]
                                            line-idx 0]
                                       (if (>= line-idx (count all-buffer-lines))
                                         positions
                                         (let [line-len (count (nth all-buffer-lines line-idx ""))
                                               next-pos (+ (peek positions) line-len 1)]  ; +1 for newline
                                           (recur (conj positions next-pos) (inc line-idx))))))
              default-color "#d4d4d4"]
          (for [[idx line] (map-indexed vector lines)]
            (let [line-number (+ viewport-start idx)
                  ;; Issue #138: For *Completions*, don't use full-line hl-line highlighting
                  ;; Instead, use span-based cursor-face highlighting
                  is-current-line? (and hl-line-enabled?
                                        owns-cursor?
                                        cursor-in-viewport?
                                        (= line-number current-line)
                                        (not is-completions-buffer?))  ; Exclude *Completions*
                  ;; Issue #137: Completion entries are hoverable/clickable
                  ;; Line 0 is header "Possible completions:", entries start at line 1
                  is-completion-entry? (and is-completions-buffer? (> line-number 0))
                  ;; Issue #138: Check if this line contains the current completion span
                  ;; Count newlines before entry start to determine which line it's on
                  entry-on-this-line? (and is-completions-buffer?
                                           owns-cursor?
                                           current-entry
                                           (= line-number (count (filter #(= % \newline)
                                                                         (take (:start current-entry)
                                                                               (or buffer-content ""))))))
                  ;; Apply whitespace visualization when enabled
                  display-line (if whitespace-enabled?
                                 (whitespace/visualize-whitespace line)
                                 line)
                  ;; Issue #130: Get line start position for font-lock
                  line-start-pos (when line-start-positions
                                   (get line-start-positions line-number 0))]
              ^{:key (str window-id "-" line-number)}  ;; Unique key per window to avoid React key collisions
              [:div.text-line
               {:class (when is-completion-entry? "completion-entry")
                :style (cond-> {:min-height (str line-height "px")
                                :color default-color}
                         ;; Add hl-line background when enabled and on current line (non-completions)
                         is-current-line? (assoc :background-color "rgba(80, 80, 120, 0.6)"))}
               ;; Issue #138: Apply span-based highlighting for *Completions* entries
               (if (and entry-on-this-line? current-entry)
                 ;; Render with cursor-face span highlighting
                 (let [line-start-in-buffer (count (clojure.string/join "\n"
                                                     (take line-number
                                                           (clojure.string/split (or buffer-content "") #"\n" -1))))
                       line-start (if (pos? line-number) (inc line-start-in-buffer) line-start-in-buffer)
                       entry-start (:start current-entry)
                       entry-end (:end current-entry)
                       rel-start (max 0 (- entry-start line-start))
                       rel-end (min (count line) (- entry-end line-start))
                       before-span (when (pos? rel-start) (subs display-line 0 rel-start))
                       highlight-span (when (and (< rel-start rel-end) (< rel-start (count display-line)))
                                        (subs display-line rel-start (min rel-end (count display-line))))
                       after-span (when (< rel-end (count display-line))
                                    (subs display-line rel-end))]
                   [:<>
                    (when before-span [:span before-span])
                    (when highlight-span
                      [:span {:style {:background-color "rgba(80, 80, 120, 0.6)"}}
                       highlight-span])
                    (when after-span [:span after-span])])
                 ;; Issue #130: Use font-lock rendering when enabled, otherwise fall back to decorations
                 (if (and font-lock-enabled? (seq face-intervals) line-start-pos)
                   (render-line-with-font-lock display-line line-number line-start-pos
                                               face-intervals current-theme decorations default-color)
                   (apply-decorations-to-line display-line line-number decorations)))]))))

      ;; Cursor - Issue #62: cursor singleton, Issue #137: block cursor
      ;; Active cursor: filled block with inverted character
      ;; Inactive cursor: hollow (border only) - shown when window doesn't own cursor
      (when cursor-pos
        (let [{:keys [line column]} cursor-pos
              ;; Calculate relative line position within viewport
              relative-line (- line (:start-line viewport 0))
              char-width 8.4
              left-padding 8
              top-padding 20
              top-px (+ top-padding (* relative-line line-height))
              left-px (+ left-padding (* column char-width))
              ;; Get the character under cursor for rendering
              cursor-char (when buffer-content
                            (let [lines (clojure.string/split buffer-content #"\n" -1)
                                  cursor-line-text (get lines line "")]
                              (if (< column (count cursor-line-text))
                                (str (nth cursor-line-text column))
                                " ")))  ; Space if at end of line
              ;; Determine if this is active (filled) or inactive (hollow) cursor
              is-active-cursor? owns-cursor?]
          (if is-active-cursor?
            ;; Active cursor: filled block with inverted character visible
            [:div.custom-cursor.cursor-active
             {:style {:position "absolute"
                      :top (str top-px "px")
                      :left (str left-px "px")
                      :width (str char-width "px")
                      :height (str line-height "px")
                      :background-color "#d4d4d4"  ; Light background
                      :color "#1e1e1e"             ; Dark text (inverted)
                      :pointer-events "none"
                      :animation "cursor-blink 1s infinite"
                      :z-index "1000"
                      :font-family "'Monaco', 'Menlo', 'Ubuntu Mono', monospace"
                      :font-size "14px"
                      :line-height (str line-height "px")
                      :text-align "center"}}
             cursor-char]
            ;; Inactive cursor: hollow block (border only)
            [:div.custom-cursor.cursor-inactive
             {:style {:position "absolute"
                      :top (str top-px "px")
                      :left (str left-px "px")
                      :width (str (- char-width 2) "px")  ; Account for border
                      :height (str (- line-height 2) "px")
                      :background-color "transparent"
                      :border "1px solid #888888"
                      :pointer-events "none"
                      :z-index "1000"}}])))]])))

(defn window-mode-line
  "Mode-line for a specific window (Issue #63)"
  [window-id is-active?]
  (let [window @(rf/subscribe [:lexicon.core.subs/window-by-id window-id])
        buffer @(rf/subscribe [:lexicon.core.subs/window-buffer window-id])
        buffer-id (:id buffer)
        cursor-pos @(rf/subscribe [:lexicon.core.subs/window-cursor-position window-id])
        show-line-numbers? @(rf/subscribe [:show-line-numbers?])
        show-column-number? @(rf/subscribe [:show-column-number?])
        mode-line-style @(rf/subscribe [:theme-face :mode-line])
        mode-line-inactive-style @(rf/subscribe [:theme-face :mode-line-inactive])
        buffer-id-style @(rf/subscribe [:theme-face :mode-line-buffer-id])
        effective-style (if is-active? mode-line-style mode-line-inactive-style)
        buffer-modified? (:is-modified? buffer false)
        ;; Issue #130: Which-func display
        which-func-display @(rf/subscribe [:which-func/mode-line-display buffer-id])]
    [:div.window-mode-line.status-bar  ; Add .status-bar for E2E test compatibility (Issue #66)
     {:style (merge effective-style
                    {:height "24px"
                     :font-size "12px"
                     :font-family "monospace"
                     :display "flex"
                     :align-items "center"
                     :padding "0 8px"
                     :border-top (str "1px solid " (:border-color effective-style "#3e3e42"))
                     :flex-shrink "0"})}

     ;; Encoding indicator (U: = UTF-8 Unix)
     [:span.encoding-info
      {:style {:margin-right "6px"}}
      "U:"]

     ;; Modified status (** = modified, -- = unmodified, %% = read-only)
     [:span.modified-info
      {:style {:margin-right "6px"}}
      (if (:is-read-only? buffer)
        (if buffer-modified? "%*" "%%")
        (if buffer-modified? "**" "--"))]

     ;; Buffer name (styled with mode-line-buffer-id face)
     [:span.buffer-info
      {:style (merge buffer-id-style {:margin-right "10px"})}
      (:name buffer)]

     ;; Issue #130: Which-func display (current function name)
     (when which-func-display
       [:span.which-func-info
        {:style {:margin-right "10px"
                 :color (:color effective-style "#888888")}}
        which-func-display])

     [:div.spacer {:style {:flex "1"}}]

     ;; Position indicator (Top/Bot/All/N%)
     [:span.position-info
      {:style {:margin-right "8px"}}
      (when cursor-pos
        (let [total-lines (get-in buffer [:cache :line-count] 1)
              current-line (:line cursor-pos)]
          (cond
            (<= total-lines 1) "All"
            (= current-line 0) "Top"
            (>= current-line (dec total-lines)) "Bot"
            :else (str (int (* 100 (/ (inc current-line) total-lines))) "%"))))]

     ;; Line number (L12 format)
     [:span.cursor-info
      (when cursor-pos
        (str (when show-line-numbers?
               (str "L" (inc (:line cursor-pos))))
             (when (and show-line-numbers? show-column-number?) " ")
             (when show-column-number?
               (str "C" (:column cursor-pos)))))]

     ;; Major mode (Emacs format: mode-name in parentheses)
     [:span.mode-info.mode-line
      {:style {:margin-left "10px"}}
      (when-let [mode (:major-mode buffer)]
        (str "(" (mode-line/get-mode-name mode (:mode-data buffer {})) ")"))
      ;; Minor mode indicators
      (let [minor-modes (filter identity
                                [(when show-line-numbers? "L")
                                 (when show-column-number? "C")])]
        (when (seq minor-modes)
          (str " (" (clojure.string/join " " minor-modes) ")")))]]))

(defn window-pane
  "Render a single window pane (leaf node) with its own mode-line (Issue #63)"
  [window-id is-active? hidden-input-ref]
  [:div.window-pane
   {:style {:display "flex"
            :flex-direction "column"  ; Changed from row to column for mode-line
            :width "100%"
            :height "100%"
            ;; Issue #137: Removed blue active border - cursor active/inactive state
            ;; now clearly indicates which window has focus
            :box-sizing "border-box"}}
   ;; Content area (gutter + text)
   [:div.window-content
    {:style {:display "flex"
             :flex-direction "row"
             :flex "1"  ; Take remaining space
             :overflow "hidden"}}
    [window-pane-gutter window-id]
    [window-pane-text window-id is-active? hidden-input-ref]]
   ;; Mode-line at bottom
   [window-mode-line window-id is-active?]])

(defn render-window-tree
  "Recursively render the window tree.
   Supports :fixed-height on leaf windows for *Completions* buffer."
  [tree active-window-id hidden-input-ref]
  (case (:type tree)
    :leaf
    [window-pane (:id tree) (= (:id tree) active-window-id) hidden-input-ref]

    :hsplit
    (let [;; Check if second child has fixed height (e.g., *Completions*)
          second-fixed-height (when (= (:type (:second tree)) :leaf)
                                (:fixed-height (:second tree)))]
      [:div.hsplit
       {:style {:display "flex"
                :flex-direction "column"
                :width "100%"
                :height "100%"}}
       [:div.split-first
        {:style (if second-fixed-height
                  ;; First takes remaining space when second is fixed
                  {:flex "1"
                   :width "100%"
                   :min-height "0"  ; Allow shrinking
                   :overflow "hidden"
                   :border-bottom "1px solid #3e3e3e"}
                  ;; Normal equal split
                  {:flex "1"
                   :width "100%"
                   :border-bottom "1px solid #3e3e3e"})}
        (render-window-tree (:first tree) active-window-id hidden-input-ref)]
       [:div.split-second
        {:style (if second-fixed-height
                  ;; Fixed height for *Completions*
                  {:flex "none"
                   :width "100%"
                   :height (str second-fixed-height "px")
                   :max-height (str second-fixed-height "px")
                   :overflow "hidden"}
                  ;; Normal equal split
                  {:flex "1"
                   :width "100%"})}
        (render-window-tree (:second tree) active-window-id hidden-input-ref)]])

    :vsplit
    [:div.vsplit
     {:style {:display "flex"
              :flex-direction "row"
              :width "100%"
              :height "100%"}}
     [:div.split-first
      {:style {:flex "1"
               :height "100%"
               :border-right "1px solid #3e3e3e"}}
      (render-window-tree (:first tree) active-window-id hidden-input-ref)]
     [:div.split-second
      {:style {:flex "1"
               :height "100%"}}
      (render-window-tree (:second tree) active-window-id hidden-input-ref)]]

    ;; Default case
    [:div.error "Unknown window tree node type"]))

(defn editor-wrapper
  "Main editor wrapper with hidden textarea + custom DOM architecture"
  []
  ;; Use r/with-let to preserve the atom across re-renders
  (r/with-let [hidden-input-ref (atom nil)]
    (let [window-tree @(rf/subscribe [:window-tree])
          active-window-id @(rf/subscribe [:active-window-id])]

      [:div.editor-wrapper
       {:tabIndex 0
        :on-click (fn [_]
                    ;; Focus the hidden input when wrapper is clicked
                    (when-let [hidden-input @hidden-input-ref]
                      (.focus hidden-input)))
        :style {:position "sticky"
                :top "0"
                :width "100%"
                :height "calc(100vh - 24px)"
                :outline "none"}}

       ;; Hidden input handler - captures all keyboard input (shared by all windows)
       [hidden-input-handler hidden-input-ref]

       ;; Recursive window tree rendering
       [:div.editor-content
        {:on-click (fn [e] (when-let [hidden-input @hidden-input-ref] (.focus hidden-input)) (.stopPropagation e))
         :style {:display "flex"
                 :flex-direction "row"
                 :width "100%"
                 :height "100%"
                 :min-height "100%"}}
        (render-window-tree window-tree active-window-id hidden-input-ref)]])))

;; Add document-level listener to capture ALL keyboard input
;; This ensures input is captured even if hidden-input briefly loses focus
(defonce ^:private keydown-listener-installed?
  (do
    (.addEventListener js/document "keydown"
                       (fn [e]
                         (let [ctrl? (.-ctrlKey e)
                               key (.-key e)
                               target (.-target e)
                               target-tag (when target (.-tagName target))
                               ;; Check if we're in an input field that should receive normal input
                               in-form-field? (and target-tag
                                                   (or (= target-tag "INPUT")
                                                       (= target-tag "SELECT")))]
                           ;; Block dangerous browser shortcuts
                           (when (and ctrl? (or (= key "w") (= key "t") (= key "n")))
                             (.preventDefault e)
                             (.stopPropagation e))

                           ;; If not in a form field, route to editor's keydown handler
                           ;; This catches input even when hidden-input loses focus
                           (when (and (not in-form-field?)
                                      ;; Don't double-handle if already on hidden-input
                                      (not (and target-tag
                                                (= target-tag "TEXTAREA")
                                                (.contains (.-classList target) "hidden-input"))))
                             ;; Refocus hidden input and handle the key
                             (when-let [hidden-input (js/document.querySelector ".hidden-input")]
                               (.focus hidden-input))
                             ;; Handle the keydown event directly
                             (handle-keydown e))))
                       #js {:capture true})
    true))

(defn editor-view
  "Main editor view with virtualized scrolling and custom cursor architecture"
  []
  (let [total-lines @(rf/subscribe [:lexicon.core.subs/total-lines])
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
               :overflow-y "hidden"  ; No scrollbar - Emacs-style (scroll via commands)
               :overflow-x "hidden"  ; Prevent horizontal scrollbar
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
  "Minibuffer component - ALWAYS VISIBLE (Emacs-compatible).

  When idle: Shows empty line or echo messages
  When active: Shows prompt + input for commands like M-x
  Expands dynamically for completions (Phase 7.8.1)

  Supports custom renderers via :custom-renderer in frame config (Issue #46)"
  []
  (r/with-let [input-ref (atom nil)
               cursor-pos-atom (r/atom 0)]  ; Track cursor position for block cursor
    (let [minibuffer @(rf/subscribe [:minibuffer])
          mode-line-style @(rf/subscribe [:theme-face :mode-line])
          icomplete-display @(rf/subscribe [:icomplete/display])
          prompt-style @(rf/subscribe [:theme-face :minibuffer-prompt])
          prefix-key-state @(rf/subscribe [:prefix-key-state])  ; Issue #137: Track prefix for C-x o
          cursor-owner @(rf/subscribe [:cursor-owner])  ; Issue #137: Check if minibuffer owns cursor
          owns-cursor? (= cursor-owner :minibuffer)
          active? (:active? minibuffer)
          message (:message minibuffer)
          height-lines (:height-lines minibuffer 1)
          line-height 24
          total-height (* height-lines line-height)
          filtered-completions (:filtered-completions minibuffer)
          show-completions? (:show-completions? minibuffer)
          completion-index (:completion-index minibuffer)
          custom-renderer (:custom-renderer minibuffer)
          renderer-props (:renderer-props minibuffer {})
          input-text (:input minibuffer "")
          cursor-pos @cursor-pos-atom]

    ;; ALWAYS render - never conditional
    [:div.minibuffer
     {:style (merge mode-line-style
                    {:position "fixed"
                     :bottom "0"
                     :left "0"
                     :right "0"
                     :height (str total-height "px")
                     :font-size "12px"
                     :font-family "monospace"
                     :display "flex"
                     :flex-direction "column"
                     :border-top (str "1px solid " (:border-color mode-line-style "#3e3e42"))
                     :z-index "1000"
                     :transition "height 0.2s ease-in-out"})}

     ;; Input line (always at top of minibuffer)
     [:div.minibuffer-input-line
      {:style {:display "flex"
               :align-items "center"
               :padding "0 8px"
               :min-height (str line-height "px")}}

      (if active?
        ;; ACTIVE MODE: Show prompt + input
        [:<>
         [:span.minibuffer-prompt
          {:style (merge prompt-style {:margin-right "4px"})}
          (:prompt minibuffer)]

         ;; Issue #137: Wrapper for input + block cursor
         [:div.minibuffer-input-wrapper
          {:style {:position "relative"
                   :flex "1"
                   :display "flex"
                   :align-items "center"}}
          [:input.minibuffer-input
           {:id "minibuffer-input"
            :ref (fn [element]
                   (when element
                     (reset! input-ref element)
                     (.focus element)
                     ;; Initialize cursor position
                     (reset! cursor-pos-atom (.-selectionStart element))))
            :type "text"
            :value input-text
            :on-change (fn [e]
                         (let [el (.-target e)]
                           (reset! cursor-pos-atom (.-selectionStart el))
                           (rf/dispatch [:minibuffer/set-input (.-value el)])))
            ;; Track cursor position on various events
            :on-click (fn [e]
                        (let [el (.-target e)]
                          (js/setTimeout #(reset! cursor-pos-atom (.-selectionStart el)) 0)))
            :on-select (fn [e]
                         (let [el (.-target e)]
                           (reset! cursor-pos-atom (.-selectionStart el))))
           :on-key-down (fn [e]
                          (let [key (.-key e)
                                ctrl? (.-ctrlKey e)
                                ;; Convert key to key-str for keybinding system
                                key-str (cond
                                          (and ctrl? (= (count key) 1))
                                          (str "C-" key)
                                          (= key " ") "SPC"
                                          (= (count key) 1) key
                                          :else nil)]
                            (cond
                              ;; Issue #137: If prefix key is active (e.g., after C-x),
                              ;; route ALL keys to keybinding system to complete the sequence
                              (and prefix-key-state key-str)
                              (do
                                (.preventDefault e)
                                (rf/dispatch [:handle-key-sequence key-str]))

                              (= key "Enter")
                              (do
                                (.preventDefault e)
                                (rf/dispatch [:minibuffer/confirm]))

                              (= key "Tab")
                              (do
                                (.preventDefault e)
                                (rf/dispatch [:minibuffer/complete]))

                              (= key "ArrowDown")
                              (do
                                (.preventDefault e)
                                ;; Use cycling events for arrow navigation
                                ;; Works with or without *Completions* buffer shown
                                (rf/dispatch [:minibuffer/cycle-next]))

                              (= key "ArrowUp")
                              (do
                                (.preventDefault e)
                                (rf/dispatch [:minibuffer/cycle-prev]))

                              (or (= key "Escape")
                                  (and ctrl? (= key "g")))
                              (do
                                (.preventDefault e)
                                (rf/dispatch (:on-cancel minibuffer)))

                              ;; Issue #137: Allow C-x prefix through to main keybinding system
                              ;; This enables C-x o (other-window) to work when minibuffer is active
                              (and ctrl? (= key "x"))
                              (do
                                (.preventDefault e)
                                (rf/dispatch [:handle-key-sequence "C-x"])))))
            :on-key-up (fn [e]
                         ;; Track cursor position after arrow key navigation
                         (let [el (.-target e)]
                           (reset! cursor-pos-atom (.-selectionStart el))))
            :style {:background-color "transparent"
                    :border "none"
                    :outline "none"
                    :color (:color mode-line-style "#cccccc")
                    :font-size "12px"
                    :font-family "monospace"
                    :flex "1"
                    ;; Issue #137: Hide native caret - we render our own block cursor
                    :caret-color "transparent"}}]
          ;; Issue #137: Block cursor for minibuffer (Emacs-style)
          (let [char-width 7.2  ; Approximate monospace character width at 12px
                cursor-left (* cursor-pos char-width)
                char-at-cursor (if (< cursor-pos (count input-text))
                                 (str (nth input-text cursor-pos))
                                 " ")]
            (if owns-cursor?
              ;; Active cursor: filled block
              [:div.minibuffer-cursor.cursor-active
               {:style {:position "absolute"
                        :left (str cursor-left "px")
                        :top "0"
                        :width (str char-width "px")
                        :height "20px"
                        :background-color "#d4d4d4"
                        :color "#1e1e1e"
                        :font-size "12px"
                        :font-family "monospace"
                        :line-height "20px"
                        :text-align "center"
                        :pointer-events "none"
                        :animation "cursor-blink 1s infinite"
                        :z-index "10"}}
               char-at-cursor]
              ;; Inactive cursor: hollow block
              [:div.minibuffer-cursor.cursor-inactive
               {:style {:position "absolute"
                        :left (str cursor-left "px")
                        :top "0"
                        :width (str (- char-width 2) "px")
                        :height "18px"
                        :background-color "transparent"
                        :border "1px solid #888888"
                        :pointer-events "none"
                        :z-index "10"}}]))]
         ;; Icomplete display (inline completion candidates)
         (when icomplete-display
           [:span.icomplete-display
            {:style {:color "#888888"
                     :font-size "12px"
                     :font-family "monospace"
                     :white-space "nowrap"}}
            icomplete-display])

         ;; Match count display [current/total] (Issue #137)
         (let [completions (:completions minibuffer [])
               total-count (count completions)
               current-pos (if (and completion-index (>= completion-index 0))
                             (inc completion-index)  ; 1-indexed for display
                             0)]                     ; 0 means no selection
           (when (pos? total-count)
             [:span.match-count
              {:style {:color "#888888"
                       :font-size "11px"
                       :font-family "monospace"
                       :margin-left "8px"
                       :white-space "nowrap"}}
              (str "[" current-pos "/" total-count "]")]))]

        ;; IDLE MODE: Show echo message or empty
        [:span.minibuffer-message.echo-area  ; Add .echo-area for E2E test compatibility (Issue #67)
         {:style {:color (:color mode-line-style "#cccccc")
                  :font-size "12px"}}
         (if (and message (not (clojure.string/blank? message)))
           message
           "")])]  ; Empty string when truly idle

     ;; Completion candidates or custom renderer
     (when active?
       (if custom-renderer
         ;; Custom renderer provided - delegate rendering to it (Issue #46)
         [:div.minibuffer-custom
          {:style {:flex "1"
                   :overflow-y "auto"
                   :padding "4px 8px"}}
          [custom-renderer (merge renderer-props
                                  {:minibuffer minibuffer
                                   :input-ref input-ref
                                   :mode-line-style mode-line-style
                                   :prompt-style prompt-style})]]

         ;; Default completion list renderer
         (when (and show-completions? (seq filtered-completions))
           [:div.minibuffer-completions
            {:style {:flex "1"
                     :overflow-y "auto"
                     :padding "4px 8px"}}
            (for [[idx candidate] (map-indexed vector (take 10 filtered-completions))]  ; Show max 10 candidates
              ^{:key idx}
              [:div.completion-candidate
               {:style {:padding "2px 4px"
                        :cursor "pointer"
                        :background-color (if (= idx completion-index)
                                            "rgba(100, 100, 200, 0.3)"  ; Highlight selected
                                            "transparent")
                        :border-radius "2px"}
                :on-click (fn []
                            (rf/dispatch [:minibuffer/select-completion idx]))}
               candidate])])))])))  ; Display completion candidate

(defn echo-area
  "DEPRECATED: Echo area now unified with minibuffer.

  This component is kept for backwards compatibility but does nothing.
  Echo messages are displayed in the minibuffer when idle."
  []
  nil)

(defn status-bar
  "DEPRECATED: Global status bar replaced with per-window mode-lines (Issue #63).
   This function is kept for reference but should not be used."
  []
  (let [cursor-pos          @(rf/subscribe [:lexicon.core.subs/cursor-position])
        buffer-length       @(rf/subscribe [:buffer-length])
        buffer-modified?    @(rf/subscribe [:buffer-modified?])
        active-buffer       @(rf/subscribe [:active-buffer])
        minibuffer          @(rf/subscribe [:minibuffer])
        show-line-numbers?  @(rf/subscribe [:show-line-numbers?])
        show-column-number? @(rf/subscribe [:show-column-number?])
        mode-line-style     @(rf/subscribe [:theme-face :mode-line])
        buffer-id-style     @(rf/subscribe [:theme-face :mode-line-buffer-id])]

    ;; ALWAYS render mode-line (positioned ABOVE minibuffer)
    [:div.status-bar
     {:style (merge mode-line-style
                    {:position    "fixed"
                     :bottom      "24px"  ; Above minibuffer
                       :left        "0"
                       :right       "0"
                       :height      "24px"
                       :font-size   "12px"
                       :font-family "monospace"
                       :display     "flex"
                       :align-items "center"
                       :padding     "0 8px"
                       :border-top  (str "1px solid " (:border-color mode-line-style "#3e3e42"))})}

     ;; Encoding indicator (U: = UTF-8 Unix)
     [:span.encoding-info
      {:style {:margin-right "6px"}}
      "U:"]

     ;; Modified status (** = modified, -- = unmodified, %% = read-only)
     [:span.modified-info
      {:style {:margin-right "6px"}}
      (if (:is-read-only? active-buffer)
        (if buffer-modified? "%*" "%%")
        (if buffer-modified? "**" "--"))]

     ;; Buffer name (styled with mode-line-buffer-id face)
     [:span.buffer-info
      {:style (merge buffer-id-style {:margin-right "10px"})}
      (:name active-buffer)]

       [:div.spacer {:style {:flex "1"}}]

       ;; Position indicator (Top/Bot/All/N%)
       [:span.position-info
        {:style {:margin-right "8px"}}
        (when cursor-pos
          (let [total-lines (get-in active-buffer [:cache :line-count] 1)
                current-line (:line cursor-pos)]
            (cond
              (<= total-lines 1) "All"
              (= current-line 0) "Top"
              (>= current-line (dec total-lines)) "Bot"
              :else (str (int (* 100 (/ (inc current-line) total-lines))) "%"))))]

       ;; Line number (L12 format)
       [:span.cursor-info
        (when cursor-pos
          (str (when show-line-numbers?
                 (str "L" (inc (:line cursor-pos))))
               (when (and show-line-numbers? show-column-number?) " ")
               (when show-column-number?
                 (str "C" (:column cursor-pos)))))]

       ;; Major mode (Emacs format: mode-name in parentheses)
       [:span.mode-info.mode-line
        {:style {:margin-left "10px"}}
        (when-let [mode (:major-mode active-buffer)]
          (str "(" (mode-line/get-mode-name mode (:mode-data active-buffer {})) ")"))
        ;; Minor mode indicators
        (let [minor-modes (filter identity
                                  [(when show-line-numbers? "L")
                                   (when show-column-number? "C")])]
          (when (seq minor-modes)
            (str " (" (clojure.string/join " " minor-modes) ")")))]]))

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
       ;; status-bar removed - now per-window mode-lines (Issue #63)
       [echo-area]
       [minibuffer-view]]

      :else
      [:div.error "Failed to initialize editor"])]]))

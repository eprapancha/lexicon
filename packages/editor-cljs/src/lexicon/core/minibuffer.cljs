(ns lexicon.core.minibuffer
  "Minibuffer stack operations for Phase 6.5 Week 3-4.

  Implements Emacs-style minibuffer stack for recursive minibuffer activation.
  Fixes Issue #72 (query-replace-regexp timeout) by allowing command-to-command
  minibuffer handoff without premature deactivation.

  Architecture:
  - :minibuffer-stack is a vector of frames
  - Each frame is a map with :prompt, :input, :on-confirm, :on-cancel, etc.
  - Empty stack [] means minibuffer is inactive
  - Stack depth = (count minibuffer-stack)")

;; =============================================================================
;; Stack Operations
;; =============================================================================

(defn minibuffer-active?
  "Check if minibuffer is currently active (stack not empty)"
  [db]
  (seq (:minibuffer-stack db)))

(defn minibuffer-depth
  "Get current minibuffer depth (0 = inactive, 1+ = active at that level)"
  [db]
  (count (:minibuffer-stack db)))

(defn current-frame
  "Get the current (topmost) minibuffer frame, or nil if inactive"
  [db]
  (last (:minibuffer-stack db)))

(defn push-frame
  "Push a new minibuffer frame onto the stack.

   Frame config keys:
   - :prompt - Prompt string (required)
   - :input - Initial input text (default \"\")
   - :on-confirm - Event vector to dispatch on RET (required)
   - :on-cancel - Event vector to dispatch on C-g (default [:minibuffer/deactivate])
   - :completions - List of completion candidates (default [])
   - :completion-index - Current selection index (default 0)
   - :metadata - Completion metadata (default nil)
   - :persist? - Don't auto-deactivate on confirm (default false)
   - :show-completions? - Show completion list (default false)
   - :filtered-completions - Filtered completion list (default [])
   - :height-lines - Minibuffer height (default 1)
   - :custom-renderer - Custom Reagent component fn for rendering (default nil)
   - :renderer-props - Props map to pass to custom renderer (default {})
   - :last-tab-input - Input when TAB was last pressed (for vanilla Emacs completion)
   - :original-input - User's typed input before arrow cycling (for restore on further typing)
   - :cycling? - True if currently cycling through completions with arrows

   Returns updated db with new frame pushed."
  [db config]
  (let [frame (merge {:prompt ""
                      :input ""
                      :on-confirm nil
                      :on-cancel [:minibuffer/deactivate]
                      :completions []
                      :predicate nil          ; Filter function for completions (Phase 7: Vertico)
                      :completion-index -1  ; -1 = user input, 0+ = cycling through completions
                      :metadata nil
                      :persist? false
                      :show-completions? false
                      :filtered-completions []
                      :height-lines 1
                      :custom-renderer nil
                      :renderer-props {}
                      :last-tab-input nil
                      :original-input ""    ; What user typed before cycling
                      :cycling? false       ; Currently cycling with arrows?
                      :annotation-fn nil    ; Function to generate annotations for completions
                      ;; Vertical completion (Phase 7: Vertico)
                      :completion-display nil          ; nil | :vertical
                      :vertical-candidates []          ; Vector of {:candidate "str" :suffix "" :group-title nil}
                      :vertical-index -1               ; -1 = prompt selected, 0+ = candidate index
                      :vertical-scroll 0               ; First visible candidate index
                      :vertical-count 10               ; Max visible candidates
                      :vertical-count-format nil       ; String like "3/42" for count display
                      ;; Multi-source narrowing (consult--multi)
                      :narrow-key nil                  ; Current narrow key character
                      :narrow-value nil                ; Current narrow source name
                      :narrow-predicate nil             ; Filter predicate based on narrow
                      ;; Preview state function
                      :state-fn nil                    ; Preview state function (consult)
                      ;; Completion table and enrichment
                      :completion-table nil             ; Original completion table
                      :default nil                     ; Default value for completing-read
                      :hist nil                        ; History symbol
                      :require-match nil}              ; Whether match is required
                     config)]
    (update db :minibuffer-stack conj frame)))

(defn pop-frame
  "Pop the current minibuffer frame from the stack.
   Returns updated db with frame removed.
   If stack becomes empty, minibuffer is deactivated.
   Returns db unchanged if stack is already empty."
  [db]
  (let [stack (:minibuffer-stack db)]
    (if (empty? stack)
      db  ; Stack is already empty, nothing to pop
      (assoc db :minibuffer-stack (pop stack)))))

(defn replace-current-frame
  "Replace the current (topmost) frame with a new frame.
   This is used for M-x → command transitions where the command
   wants to take over the minibuffer slot without nesting.

   Returns updated db with current frame replaced."
  [db config]
  (let [stack (:minibuffer-stack db)]
    (if (empty? stack)
      ;; No frame to replace - just push
      (push-frame db config)
      ;; Replace top frame
      (let [new-stack (pop stack)
            frame (merge {:prompt ""
                          :input ""
                          :on-confirm nil
                          :on-cancel [:minibuffer/deactivate]
                          :completions []
                          :completion-index -1
                          :metadata nil
                          :persist? false
                          :show-completions? false
                          :filtered-completions []
                          :height-lines 1
                          :custom-renderer nil
                          :renderer-props {}
                          :last-tab-input nil
                          :original-input ""
                          :cycling? false
                          :annotation-fn nil
                          ;; Vertical completion (Phase 7: Vertico)
                          :completion-display nil
                          :vertical-candidates []
                          :vertical-index -1
                          :vertical-scroll 0
                          :vertical-count 10
                          :vertical-count-format nil
                          ;; Multi-source narrowing
                          :narrow-key nil
                          :narrow-value nil
                          :narrow-predicate nil
                          ;; Preview and enrichment
                          :state-fn nil
                          :completion-table nil
                          :default nil
                          :hist nil
                          :require-match nil}
                         config)]
        (assoc db :minibuffer-stack (conj new-stack frame))))))

(defn update-current-frame
  "Update the current (topmost) minibuffer frame with new data.

   Updates can be a map of key-value pairs to merge into current frame.
   Returns updated db, or unchanged db if no frame is active."
  [db updates]
  (let [stack (:minibuffer-stack db)]
    (if (empty? stack)
      db  ; No frame to update
      (let [current (last stack)
            updated-frame (merge current updates)
            new-stack (conj (pop stack) updated-frame)]
        (assoc db :minibuffer-stack new-stack)))))

;; =============================================================================
;; Accessor Functions - Read from current frame
;; =============================================================================

(defn get-input
  "Get input from current frame, or empty string if no frame"
  [db]
  (:input (current-frame db) ""))

(defn get-prompt
  "Get prompt from current frame"
  [db]
  (:prompt (current-frame db) ""))

(defn get-on-confirm
  "Get on-confirm handler from current frame"
  [db]
  (:on-confirm (current-frame db)))

(defn get-on-cancel
  "Get on-cancel handler from current frame"
  [db]
  (:on-cancel (current-frame db) [:minibuffer/deactivate]))

(defn get-on-change
  "Get on-change handler from current frame"
  [db]
  (:on-change (current-frame db)))

(defn get-persist?
  "Get persist? flag from current frame"
  [db]
  (:persist? (current-frame db) false))

(defn get-completions
  "Get completions list from current frame"
  [db]
  (:completions (current-frame db) []))

(defn get-predicate
  "Get completion predicate function from current frame (Phase 7: Vertico)"
  [db]
  (:predicate (current-frame db)))

(defn get-completion-index
  "Get completion-index from current frame"
  [db]
  (:completion-index (current-frame db) -1))

(defn get-completion-metadata
  "Get completion metadata from current frame"
  [db]
  (:metadata (current-frame db)))

(defn get-filtered-completions
  "Get filtered completions from current frame"
  [db]
  (:filtered-completions (current-frame db) []))

(defn get-show-completions?
  "Get show-completions? flag from current frame"
  [db]
  (:show-completions? (current-frame db) false))

(defn get-height-lines
  "Get height-lines from current frame"
  [db]
  (:height-lines (current-frame db) 1))

(defn get-custom-renderer
  "Get custom-renderer from current frame"
  [db]
  (:custom-renderer (current-frame db)))

(defn get-renderer-props
  "Get renderer-props from current frame"
  [db]
  (:renderer-props (current-frame db) {}))

(defn get-last-tab-input
  "Get last-tab-input from current frame"
  [db]
  (:last-tab-input (current-frame db)))

(defn get-original-input
  "Get original-input from current frame"
  [db]
  (:original-input (current-frame db) ""))

(defn get-cycling?
  "Get cycling? flag from current frame"
  [db]
  (:cycling? (current-frame db) false))

(defn get-annotation-fn
  "Get annotation-fn from current frame"
  [db]
  (:annotation-fn (current-frame db)))

;; =============================================================================
;; Setter Functions - Update current frame
;; =============================================================================

(defn set-input
  "Set input in current frame"
  [db value]
  (update-current-frame db {:input value}))

(defn set-completions
  "Set completions in current frame"
  [db value]
  (update-current-frame db {:completions value}))

(defn set-completion-index
  "Set completion-index in current frame"
  [db value]
  (update-current-frame db {:completion-index value}))

(defn set-filtered-completions
  "Set filtered-completions in current frame"
  [db value]
  (update-current-frame db {:filtered-completions value}))

(defn set-show-completions?
  "Set show-completions? in current frame"
  [db value]
  (update-current-frame db {:show-completions? value}))

(defn set-height-lines
  "Set height-lines in current frame"
  [db value]
  (update-current-frame db {:height-lines value}))

(defn set-last-tab-input
  "Set last-tab-input in current frame"
  [db value]
  (update-current-frame db {:last-tab-input value}))

(defn set-original-input
  "Set original-input in current frame"
  [db value]
  (update-current-frame db {:original-input value}))

(defn set-cycling?
  "Set cycling? flag in current frame"
  [db value]
  (update-current-frame db {:cycling? value}))

(defn set-metadata
  "Set completion metadata in current frame"
  [db value]
  (update-current-frame db {:metadata value}))

(defn get-completion-table
  "Get completion-table from current frame"
  [db]
  (:completion-table (current-frame db)))

(defn set-completion-table
  "Set completion-table in current frame"
  [db value]
  (update-current-frame db {:completion-table value}))

;; =============================================================================
;; Vertical Completion Accessors/Setters (Phase 7: Vertico)
;; =============================================================================

(defn get-completion-display
  "Get completion-display mode from current frame (:vertical or nil)"
  [db]
  (:completion-display (current-frame db)))

(defn set-completion-display
  "Set completion-display mode in current frame"
  [db value]
  (update-current-frame db {:completion-display value}))

(defn get-vertical-candidates
  "Get vertical-candidates from current frame"
  [db]
  (:vertical-candidates (current-frame db) []))

(defn set-vertical-candidates
  "Set vertical-candidates in current frame"
  [db value]
  (update-current-frame db {:vertical-candidates value}))

(defn get-vertical-index
  "Get vertical-index from current frame (-1 = prompt, 0+ = candidate)"
  [db]
  (:vertical-index (current-frame db) -1))

(defn set-vertical-index
  "Set vertical-index in current frame"
  [db value]
  (update-current-frame db {:vertical-index value}))

(defn get-vertical-scroll
  "Get vertical-scroll (first visible candidate index) from current frame"
  [db]
  (:vertical-scroll (current-frame db) 0))

(defn set-vertical-scroll
  "Set vertical-scroll in current frame"
  [db value]
  (update-current-frame db {:vertical-scroll value}))

(defn get-vertical-count
  "Get vertical-count (max visible candidates) from current frame"
  [db]
  (:vertical-count (current-frame db) 10))

(defn set-vertical-count
  "Set vertical-count in current frame"
  [db value]
  (update-current-frame db {:vertical-count value}))

(defn get-vertical-count-format
  "Get vertical-count-format string (e.g., '3/42') from current frame"
  [db]
  (:vertical-count-format (current-frame db)))

(defn set-vertical-count-format
  "Set vertical-count-format in current frame"
  [db value]
  (update-current-frame db {:vertical-count-format value}))

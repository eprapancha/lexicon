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
                      :annotation-fn nil}   ; Function to generate annotations for completions
                     config)]
    (update db :minibuffer-stack conj frame)))

(defn pop-frame
  "Pop the current minibuffer frame from the stack.
   Returns updated db with frame removed.
   If stack becomes empty, minibuffer is deactivated."
  [db]
  (let [stack (:minibuffer-stack db)
        new-stack (pop stack)]
    (assoc db :minibuffer-stack new-stack)))

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
                          :annotation-fn nil}
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
;; Backward Compatibility Helpers
;; =============================================================================

(defn sync-to-legacy
  "Sync the current frame to the legacy :minibuffer map for backward compatibility.
   This allows old code to continue reading from :minibuffer while we migrate."
  [db]
  (if-let [frame (current-frame db)]
    (assoc db :minibuffer (merge (:minibuffer db)
                                  {:active? true
                                   :prompt (:prompt frame)
                                   :input (:input frame)
                                   :on-confirm (:on-confirm frame)
                                   :on-cancel (:on-cancel frame)
                                   :completions (:completions frame)
                                   :completion-index (:completion-index frame)
                                   :completion-metadata (:metadata frame)
                                   :persist? (:persist? frame)
                                   :show-completions? (:show-completions? frame)
                                   :filtered-completions (:filtered-completions frame)
                                   :height-lines (:height-lines frame)
                                   :custom-renderer (:custom-renderer frame)
                                   :renderer-props (:renderer-props frame)
                                   :last-tab-input (:last-tab-input frame)
                                   :original-input (:original-input frame)
                                   :cycling? (:cycling? frame)
                                   :annotation-fn (:annotation-fn frame)}))
    ;; No frame - mark as inactive
    (assoc-in db [:minibuffer :active?] false)))

;; =============================================================================
;; Helper Functions for Event Handlers
;; =============================================================================

(defn get-input
  "Get input from current frame, or empty string if no frame"
  [db]
  (:input (current-frame db) ""))

(defn get-on-confirm
  "Get on-confirm handler from current frame"
  [db]
  (:on-confirm (current-frame db)))

(defn get-persist?
  "Get persist? flag from current frame"
  [db]
  (:persist? (current-frame db) false))

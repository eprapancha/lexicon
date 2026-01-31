(ns lexicon.core.icomplete
  "Incremental completion feedback in the minibuffer (icomplete).

  When icomplete-mode is enabled, prospective completions are shown
  inline in the minibuffer as you type.

  Display format:
  - {...} - multiple candidates, separated by icomplete-separator
  - (...) - single match, matching is required
  - [...] - single match, matching is optional

  Key bindings (when minibuffer active):
  - C-. : icomplete-forward-completions (cycle to next)
  - C-, : icomplete-backward-completions (cycle to previous)

  Based on Emacs lisp/icomplete.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]
            [lexicon.core.completion.styles :as styles]))

;; =============================================================================
;; Configuration
;; =============================================================================

(defonce icomplete-separator (atom " | "))
(defonce icomplete-max-candidates (atom 5))
(defonce icomplete-show-matches-on-no-input (atom false))

;; =============================================================================
;; State
;; =============================================================================

;; Whether icomplete-mode is enabled
(defonce icomplete-mode-enabled? (atom false))

;; Current completion index for cycling
(defonce icomplete-index (atom 0))

;; Cached completions for current input
(defonce icomplete-cached-completions (atom []))

;; Last input we computed completions for
(defonce icomplete-last-input (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn get-completions
  "Get filtered completions for the current minibuffer input."
  [input all-completions]
  (if (str/blank? input)
    (if @icomplete-show-matches-on-no-input
      (vec (take 100 all-completions))
      [])
    ;; Use completion styles to filter
    (let [style-fns [:basic :substring :flex]
          matches (loop [fns style-fns]
                    (if (empty? fns)
                      []
                      (let [style (first fns)
                            style-fn (get styles/completion-styles style)
                            results (when style-fn
                                      (filter #(style-fn input %) all-completions))]
                        (if (seq results)
                          (vec results)
                          (recur (rest fns))))))]
      (vec (take 100 matches)))))

(defn format-icomplete-display
  "Format the icomplete display string showing candidates.
   Returns a string like '{foo | bar | baz}' or '(single-match)'."
  [completions current-index require-match?]
  (cond
    (empty? completions)
    " [No matches]"

    (= 1 (count completions))
    (let [brackets (if require-match? ["(" ")"] ["[" "]"])]
      (str " " (first brackets) (first completions) (second brackets)))

    :else
    (let [;; Rotate completions so current index is first
          n (count completions)
          rotated (if (pos? current-index)
                    (vec (concat (drop current-index completions)
                                 (take current-index completions)))
                    completions)
          ;; Take only max-candidates to display
          displayed (take @icomplete-max-candidates rotated)
          more? (> n @icomplete-max-candidates)
          candidates-str (str/join @icomplete-separator displayed)
          suffix (when more? ",...")]
      (str " {" candidates-str suffix "}"))))

(defn reset-icomplete-state!
  "Reset icomplete state for a new minibuffer session."
  []
  (reset! icomplete-index 0)
  (reset! icomplete-cached-completions [])
  (reset! icomplete-last-input nil))

;; =============================================================================
;; Re-frame Subscriptions
;; =============================================================================

(rf/reg-sub
 :icomplete/display
 (fn [db _]
   (when @icomplete-mode-enabled?
     (let [minibuffer (:minibuffer db)
           active? (:active minibuffer)
           input (or (:input minibuffer) "")
           completions (or (:completions minibuffer) [])]
       (when active?
         ;; Check if input changed, recompute completions if so
         (when (not= input @icomplete-last-input)
           (reset! icomplete-last-input input)
           (reset! icomplete-index 0)
           (reset! icomplete-cached-completions (get-completions input completions)))
         ;; Format display
         (when (or (seq @icomplete-cached-completions)
                   (and (not (str/blank? input))
                        (seq completions)))
           (format-icomplete-display
            @icomplete-cached-completions
            @icomplete-index
            true)))))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :icomplete/forward-completions
 (fn [{:keys [db]} [_]]
   "Cycle forward through icomplete candidates (C-.)."
   (when @icomplete-mode-enabled?
     (let [n (count @icomplete-cached-completions)]
       (when (pos? n)
         (swap! icomplete-index #(mod (inc %) n))
         ;; Update minibuffer input to selected completion
         (let [selected (nth @icomplete-cached-completions @icomplete-index)]
           {:db (assoc-in db [:minibuffer :input] selected)}))))))

(rf/reg-event-fx
 :icomplete/backward-completions
 (fn [{:keys [db]} [_]]
   "Cycle backward through icomplete candidates (C-,)."
   (when @icomplete-mode-enabled?
     (let [n (count @icomplete-cached-completions)]
       (when (pos? n)
         (swap! icomplete-index #(mod (dec %) n))
         ;; Update minibuffer input to selected completion
         (let [selected (nth @icomplete-cached-completions @icomplete-index)]
           {:db (assoc-in db [:minibuffer :input] selected)}))))))

(rf/reg-event-fx
 :icomplete/update-completions
 (fn [{:keys [db]} [_]]
   "Update icomplete completions when minibuffer input changes."
   (when @icomplete-mode-enabled?
     (let [minibuffer (:minibuffer db)
           input (or (:input minibuffer) "")
           completions (or (:completions minibuffer) [])]
       (when (not= input @icomplete-last-input)
         (reset! icomplete-last-input input)
         (reset! icomplete-index 0)
         (reset! icomplete-cached-completions (get-completions input completions)))))
   {:db db}))

(rf/reg-event-fx
 :icomplete-mode
 (fn [{:keys [db]} [_ enable?]]
   "Toggle or set icomplete-mode.
    With no argument, toggle. With argument, enable if truthy."
   (let [new-state (if (nil? enable?)
                     (not @icomplete-mode-enabled?)
                     (boolean enable?))]
     (reset! icomplete-mode-enabled? new-state)
     (when new-state
       (reset-icomplete-state!))
     {:db db
      :fx [[:dispatch [:echo/message (if new-state
                                       "Icomplete mode enabled"
                                       "Icomplete mode disabled")]]]})))

;; =============================================================================
;; Minibuffer Integration
;; =============================================================================

;; Hook into minibuffer activation to reset state
(rf/reg-event-fx
 :icomplete/minibuffer-setup
 (fn [{:keys [db]} [_]]
   (when @icomplete-mode-enabled?
     (reset-icomplete-state!))
   {:db db}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-icomplete!
  "Initialize icomplete mode commands and keybindings."
  []
  ;; Register icomplete-mode command
  (rf/dispatch [:register-command :icomplete-mode
                {:docstring "Toggle incremental completion feedback in minibuffer"
                 :handler [:icomplete-mode nil]}])

  ;; Register cycling commands
  (rf/dispatch [:register-command :icomplete-forward-completions
                {:docstring "Cycle forward through icomplete candidates (C-.)"
                 :handler [:icomplete/forward-completions]}])

  (rf/dispatch [:register-command :icomplete-backward-completions
                {:docstring "Cycle backward through icomplete candidates (C-,)"
                 :handler [:icomplete/backward-completions]}])

  ;; Set up key bindings (these are active when minibuffer is active)
  ;; Note: These need to be handled specially in the minibuffer keymap
  (rf/dispatch [:keymap/set-global "C-." :icomplete-forward-completions])
  (rf/dispatch [:keymap/set-global "C-," :icomplete-backward-completions]))

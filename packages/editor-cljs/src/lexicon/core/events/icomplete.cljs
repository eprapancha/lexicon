(ns lexicon.core.events.icomplete
  "Icomplete UI integration events and subscriptions.

  This module provides the re-frame infrastructure for icomplete-mode.
  The actual completion logic lives in lexicon.packages.icomplete.

  Icomplete mode shows completion candidates inline in the minibuffer
  as you type, providing immediate feedback."
  (:require [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [clojure.string :as str]))

;; =============================================================================
;; State (stored in re-frame db under :icomplete)
;; =============================================================================

;; State is stored in db at [:icomplete ...]
;; - :enabled? - whether icomplete-mode is on
;; - :index - current completion index for cycling
;; - :cached-completions - filtered completions for current input
;; - :last-input - last input we computed completions for

;; =============================================================================
;; Completion Styles (pure functions)
;; =============================================================================

(defn- basic-style-match?
  "Basic completion style - prefix matching."
  [input candidate]
  (str/starts-with? (str/lower-case candidate) (str/lower-case input)))

(defn- substring-style-match?
  "Substring completion style - match anywhere in string."
  [input candidate]
  (str/includes? (str/lower-case candidate) (str/lower-case input)))

(defn- flex-style-match?
  "Flex completion style - characters must appear in order."
  [input candidate]
  (let [input-lower (str/lower-case input)
        candidate-lower (str/lower-case candidate)]
    (loop [i 0
           c 0]
      (cond
        (>= i (count input-lower)) true
        (>= c (count candidate-lower)) false
        (= (nth input-lower i) (nth candidate-lower c))
        (recur (inc i) (inc c))
        :else
        (recur i (inc c))))))

(def ^:private completion-styles
  {:basic basic-style-match?
   :substring substring-style-match?
   :flex flex-style-match?})

;; =============================================================================
;; Helper Functions
;; =============================================================================

(def icomplete-separator " | ")
(def icomplete-max-candidates 5)
(def icomplete-show-matches-on-no-input false)

(defn get-completions
  "Get filtered completions for input against all-completions."
  [input all-completions]
  (if (str/blank? input)
    (if icomplete-show-matches-on-no-input
      (vec (take 100 all-completions))
      [])
    ;; Use completion styles to filter
    (let [style-fns [:basic :substring :flex]
          matches (loop [fns style-fns]
                    (if (empty? fns)
                      []
                      (let [style (first fns)
                            style-fn (get completion-styles style)
                            results (when style-fn
                                      (filter #(style-fn input %) all-completions))]
                        (if (seq results)
                          (vec results)
                          (recur (rest fns))))))]
      (vec (take 100 matches)))))

(defn format-icomplete-display
  "Format the icomplete display string showing candidates."
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
          displayed (take icomplete-max-candidates rotated)
          more? (> n icomplete-max-candidates)
          candidates-str (str/join icomplete-separator displayed)
          suffix (when more? ",...")]
      (str " {" candidates-str suffix "}"))))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :icomplete/enabled?
 (fn [db _]
   (get-in db [:icomplete :enabled?] false)))

(rf/reg-sub
 :icomplete/display
 (fn [db _]
   (when (get-in db [:icomplete :enabled?])
     (let [minibuffer (:minibuffer db)
           active? (:active minibuffer)
           input (or (:input minibuffer) "")
           completions (or (:completions minibuffer) [])
           cached-input (get-in db [:icomplete :last-input])
           cached-completions (get-in db [:icomplete :cached-completions] [])
           index (get-in db [:icomplete :index] 0)]
       (when active?
         ;; Use cached completions if input unchanged
         (let [current-completions (if (= input cached-input)
                                     cached-completions
                                     (get-completions input completions))]
           (when (or (seq current-completions)
                     (and (not (str/blank? input))
                          (seq completions)))
             (format-icomplete-display current-completions index true))))))))

;; =============================================================================
;; Events
;; =============================================================================

(rf/reg-event-db
 :icomplete/enable
 (fn [db [_ enable?]]
   (let [new-state (if (nil? enable?)
                     (not (get-in db [:icomplete :enabled?]))
                     (boolean enable?))]
     (-> db
         (assoc-in [:icomplete :enabled?] new-state)
         (assoc-in [:icomplete :index] 0)
         (assoc-in [:icomplete :cached-completions] [])
         (assoc-in [:icomplete :last-input] nil)))))

(rf/reg-event-db
 :icomplete/forward-completions
 (fn [db [_]]
   (if-not (get-in db [:icomplete :enabled?])
     db
     (let [cached (get-in db [:icomplete :cached-completions] [])
           n (count cached)
           index (get-in db [:icomplete :index] 0)]
       (if (zero? n)
         db
         (let [new-index (mod (inc index) n)
               selected (nth cached new-index)]
           (-> db
               (assoc-in [:icomplete :index] new-index)
               (assoc-in [:minibuffer :input] selected))))))))

(rf/reg-event-db
 :icomplete/backward-completions
 (fn [db [_]]
   (if-not (get-in db [:icomplete :enabled?])
     db
     (let [cached (get-in db [:icomplete :cached-completions] [])
           n (count cached)
           index (get-in db [:icomplete :index] 0)]
       (if (zero? n)
         db
         (let [new-index (mod (dec index) n)
               selected (nth cached new-index)]
           (-> db
               (assoc-in [:icomplete :index] new-index)
               (assoc-in [:minibuffer :input] selected))))))))

(rf/reg-event-db
 :icomplete/update-completions
 (fn [db [_]]
   (if-not (get-in db [:icomplete :enabled?])
     db
     (let [minibuffer (:minibuffer db)
           input (or (:input minibuffer) "")
           completions (or (:completions minibuffer) [])
           last-input (get-in db [:icomplete :last-input])]
       (if (= input last-input)
         db
         (-> db
             (assoc-in [:icomplete :last-input] input)
             (assoc-in [:icomplete :index] 0)
             (assoc-in [:icomplete :cached-completions]
                       (get-completions input completions))))))))

(rf/reg-event-db
 :icomplete/minibuffer-setup
 (fn [db [_]]
   (if-not (get-in db [:icomplete :enabled?])
     db
     (-> db
         (assoc-in [:icomplete :index] 0)
         (assoc-in [:icomplete :cached-completions] [])
         (assoc-in [:icomplete :last-input] nil)))))

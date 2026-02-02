(ns lexicon.core.commands.universal-argument
  "Prefix argument system (C-u) - Phase 6.5 Week 1-2

  Implements Emacs-compatible prefix argument accumulation and conversion.
  Based on Emacs implementation in lisp/simple.el:5511-5565 and src/callint.c:648-659

  Value Types:
  - nil          → No prefix argument (default)
  - (4)          → Single C-u
  - (16)         → C-u C-u
  - (64)         → C-u C-u C-u
  - 5            → C-u 5
  - -            → C-u - (symbol)
  - -5           → C-u - 5

  Interactive Spec Codes:
  - \"P\" → Pass raw prefix-arg as-is
  - \"p\" → Pass numeric value via prefix-numeric-value

  Ownership: :prefix-arg and :current-prefix-arg owned by command.cljs

  See: Issue #76, docs/ARCHITECTURE.md"
  (:require [re-frame.core :as rf]))

;; ============================================================================
;; Prefix Argument Conversion
;; ============================================================================

(defn prefix-numeric-value
  "Convert prefix-arg to numeric value for commands using (interactive \"p\").

  Based on Emacs Fprefix_numeric_value in src/callint.c:656

  Conversion rules:
  - nil       → 1
  - (4)       → 4
  - (16)      → 16
  - '-        → -1
  - number    → number (as-is)

  Args:
    arg - Raw prefix argument value

  Returns:
    Integer numeric value"
  [arg]
  (cond
    (nil? arg) 1
    (list? arg) (first arg)
    (= arg '-) -1
    (number? arg) arg
    :else 1))

;; ============================================================================
;; Event Handlers (State Ownership: command.cljs)
;; ============================================================================

(rf/reg-event-db
  :set-prefix-arg
  (fn [db [_ value]]
    (assoc db :prefix-arg value)))

(rf/reg-event-db
  :clear-prefix-arg
  (fn [db [_]]
    (assoc db
           :prefix-arg nil
           :current-prefix-arg nil
           :transient-keymap nil)))

(rf/reg-event-db
  :save-prefix-arg
  (fn [db [_]]
    (assoc db :current-prefix-arg (:prefix-arg db))))

(rf/reg-event-db
  :set-transient-keymap
  (fn [db [_ keymap]]
    (assoc db :transient-keymap keymap)))

;; ============================================================================
;; Universal Argument Commands
;; ============================================================================

(rf/reg-event-fx
  :universal-argument
  (fn [{:keys [db]} [_]]
    {:db (assoc db :prefix-arg (list 4))
     :fx [[:dispatch [:set-transient-keymap :universal-argument-map]]]}))

(rf/reg-event-fx
  :universal-argument-more
  (fn [{:keys [db]} [_]]
    (let [arg (:prefix-arg db)
          new-arg (cond
                    ;; C-u C-u → (16), C-u C-u C-u → (64)
                    (list? arg) (list (* 4 (first arg)))
                    ;; C-u - C-u → (-4)
                    (= arg '-) (list -4)
                    ;; Already a number, keep it
                    :else arg)]
      (cond-> {:db (assoc db :prefix-arg new-arg)}
        ;; Only set transient keymap when accumulating multiplier
        (list? new-arg) (assoc :fx [[:dispatch [:set-transient-keymap :universal-argument-map]]])))))

(rf/reg-event-fx
  :digit-argument
  (fn [{:keys [db]} [_ digit]]
    (let [arg (:prefix-arg db)
          new-arg (cond
                    ;; C-u 5 → 5, C-u 5 2 → 52
                    (number? arg) (+ (* arg 10) (if (< arg 0) (- digit) digit))
                    ;; C-u - 0 → Keep '- if digit is 0, else start negative number
                    (= arg '-) (if (zero? digit) '- (- digit))
                    ;; First digit after C-u
                    :else digit)]
      {:db (assoc db :prefix-arg new-arg)
       :fx [[:dispatch [:set-transient-keymap :universal-argument-map]]]})))

(rf/reg-event-fx
  :negative-argument
  (fn [{:keys [db]} [_]]
    (let [arg (:prefix-arg db)
          new-arg (cond
                    ;; C-u - 5 - → negate
                    (number? arg) (- arg)
                    ;; C-u - - → cancel back to nil
                    (= arg '-) nil
                    ;; C-u - → '-
                    :else '-)]
      {:db (assoc db :prefix-arg new-arg)
       :fx [[:dispatch [:set-transient-keymap :universal-argument-map]]]})))

;; ============================================================================
;; Subscriptions
;; ============================================================================

(rf/reg-sub
  :prefix-arg
  (fn [db _]
    (:prefix-arg db)))

(rf/reg-sub
  :current-prefix-arg
  (fn [db _]
    (:current-prefix-arg db)))

(rf/reg-sub
  :transient-keymap
  (fn [db _]
    (:transient-keymap db)))

;; ============================================================================
;; Prefix Argument Description (for mode-line display)
;; ============================================================================

(defn prefix-arg-description
  "Return human-readable description of prefix-arg for mode-line.

  Based on Emacs universal-argument--description in lisp/simple.el:5451-5462

  Examples:
  - nil    → \"\"
  - (4)    → \"C-u\"
  - (16)   → \"C-u C-u\"
  - 5      → \"C-u 5\"
  - '-     → \"C-u -\"
  - -5     → \"C-u -5\"

  Args:
    prefix-arg - Raw prefix argument value

  Returns:
    String description for display"
  [prefix-arg]
  (when prefix-arg
    (cond
      (= prefix-arg '-) "C-u -"
      (list? prefix-arg)
      (let [n (first prefix-arg)
            cu-count (loop [val n count 0]
                       (if (and (> val 4) (zero? (mod val 4)))
                         (recur (/ val 4) (inc count))
                         count))]
        (if (= n 4)
          "C-u"
          (str (apply str (repeat (inc cu-count) "C-u ")) n)))
      :else (str "C-u " prefix-arg))))

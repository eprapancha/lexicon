(ns lexicon.lisp.timer-test
  "Lisp API tests for the timer system.

  Tests timer-related Lisp functions:
  - run-with-timer: Create one-shot and repeating timers
  - run-with-idle-timer: Create idle timers
  - cancel-timer: Cancel pending timers
  - run-at-time: Alias for run-with-timer

  JUSTIFICATION: Timer callbacks are asynchronous and require JS setTimeout
  integration. They must be tested via Lisp evaluation to verify the callback
  fires correctly after the specified delay."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; One-shot Timers
;; =============================================================================

(deftest test-run-with-timer-fires
  (testing "run-with-timer fires callback after delay"
    (lisp/setup-test)
    ;; Set up a variable to track timer firing
    (lisp/eval-lisp! "(setq timer-fired false)")
    ;; Create a timer that fires after 0.5 seconds
    (lisp/eval-lisp! "(run-with-timer 0.5 nil (fn [] (setq timer-fired true)))")
    ;; Timer should not have fired yet
    (is (false? (lisp/eval-lisp! "timer-fired"))
        "Timer should not fire immediately")
    ;; Wait for timer to fire
    (Thread/sleep 800)
    ;; Timer should have fired
    (is (true? (lisp/eval-lisp! "timer-fired"))
        "Timer should fire after delay")))

(deftest test-run-with-timer-returns-timer-id
  (testing "run-with-timer returns a timer ID"
    (lisp/setup-test)
    (let [timer-id (lisp/eval-lisp! "(run-with-timer 10 nil (fn [] nil))")]
      (is (some? timer-id) "Should return a timer ID")
      ;; Clean up
      (lisp/eval-lisp! (str "(cancel-timer " (pr-str timer-id) ")")))))

;; =============================================================================
;; Timer Cancellation
;; =============================================================================

(deftest test-cancel-timer
  (testing "cancel-timer prevents callback from firing"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq cancel-test-fired false)")
    ;; Create a timer and immediately cancel it
    (lisp/eval-lisp!
     "(let [tid (run-with-timer 0.3 nil (fn [] (setq cancel-test-fired true)))]
        (cancel-timer tid))")
    ;; Wait longer than the timer delay
    (Thread/sleep 600)
    ;; Timer should NOT have fired
    (is (false? (lisp/eval-lisp! "cancel-test-fired"))
        "Cancelled timer should not fire")))

;; =============================================================================
;; run-at-time Alias
;; =============================================================================

(deftest test-run-at-time-alias
  (testing "run-at-time works as alias for run-with-timer"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq at-time-fired false)")
    (lisp/eval-lisp! "(run-at-time 0.3 nil (fn [] (setq at-time-fired true)))")
    (Thread/sleep 600)
    (is (true? (lisp/eval-lisp! "at-time-fired"))
        "run-at-time should fire callback")))

;; =============================================================================
;; Timer with Arguments
;; =============================================================================

(deftest test-timer-with-message
  (testing "Timer callback can call message"
    (lisp/setup-test)
    (lisp/eval-lisp! "(run-with-timer 0.2 nil (fn [] (message \"timer fired!\")))")
    (Thread/sleep 500)
    (is (= "timer fired!" (lisp/eval-lisp! "(current-message)"))
        "Timer should be able to set echo area message")))

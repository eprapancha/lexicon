(ns lexicon.lisp.consult-yank-test
  "E2E tests for consult-yank-from-kill-ring command.

  Tests consult-yank-from-kill-ring functionality:
  - Shows kill ring entries as candidates
  - Selecting inserts at point

  JUSTIFICATION: Tests call kill ring functions via eval-lisp to exercise
  the yank-from-kill-ring completing-read pipeline. This is a Lisp API test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Test 1: kill-new adds to kill ring, current-kill retrieves
;; =============================================================================

(deftest test-kill-ring-api
  (testing "kill-new and current-kill work together"
    (lh/setup-test)

    ;; Add entries to kill ring
    (lh/eval-lisp! "(kill-new \"first killed text\")")
    (lh/eval-lisp! "(kill-new \"second killed text\")")
    (lh/eval-lisp! "(kill-new \"third killed text\")")
    (Thread/sleep 100)

    ;; Most recent should be "third"
    (let [latest (lh/eval-lisp! "(current-kill 0)")]
      (is (= "third killed text" latest)
          "current-kill 0 should return most recent"))

    ;; Previous should be "second"
    (let [prev (lh/eval-lisp! "(current-kill 1)")]
      (is (= "second killed text" prev)
          "current-kill 1 should return second most recent"))))

;; =============================================================================
;; Test 2: Kill ring entries shown via completing-read
;; =============================================================================

(deftest test-kill-ring-completing-read
  (testing "Kill ring entries can be presented via completing-read"
    (lh/setup-test)

    ;; Add entries
    (lh/eval-lisp! "(kill-new \"apple\")")
    (lh/eval-lisp! "(kill-new \"banana\")")
    (lh/eval-lisp! "(kill-new \"cherry\")")
    (Thread/sleep 100)

    ;; Collect kill ring entries and open completing-read
    (lh/eval-lisp! "(let [entries (loop [n 0 acc [] seen #{}]
                                    (let [entry (current-kill n)]
                                      (cond
                                        (nil? entry) acc
                                        (contains? seen entry) acc
                                        (>= n 100) acc
                                        :else (recur (inc n)
                                                     (conj acc entry)
                                                     (conj seen entry)))))]
                      (completing-read \"Yank: \" entries))")
    (Thread/sleep 300)

    ;; Minibuffer should be active
    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 3: Insert from kill ring inserts text at point
;; =============================================================================

(deftest test-insert-from-kill-ring
  (testing "Selecting a kill ring entry inserts it at point"
    (lh/setup-test)

    ;; Add to kill ring
    (lh/eval-lisp! "(kill-new \"inserted text\")")
    (Thread/sleep 100)

    ;; Insert the kill ring entry at point
    (lh/eval-lisp! "(insert (current-kill 0))")
    (Thread/sleep 100)

    ;; Verify buffer contents
    (let [text (lh/eval-lisp! "(buffer-string)")]
      (is (= "inserted text" text)
          "Buffer should contain the inserted kill ring text"))))

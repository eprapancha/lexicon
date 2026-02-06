(ns lexicon.lisp.read-only-test
  "Lisp API tests for read-only buffer and inhibit-read-only.

  Tests Lisp functions for programmatic access to read-only buffers:
  - inhibit-read-only: Query current inhibit state
  - with-inhibit-read-only: Execute code with read-only bypassed

  JUSTIFICATION: inhibit-read-only is a Lisp API feature for programmatic
  buffer modification. Testing it requires Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; inhibit-read-only API Tests
;; =============================================================================

(deftest test-inhibit-read-only-allows-lisp-insert
  (testing "Programmatic insert succeeds with inhibit-read-only"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types initial text
    (h/type-text "Initial")
    (Thread/sleep 100)

    ;; Make buffer read-only via keyboard (C-x C-q = toggle-read-only)
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Try normal insert - should fail
    (let [result (lisp/eval-lisp "(insert \" BLOCKED\")")]
      (is (not (:success result)) "Normal insert should fail in read-only buffer"))

    ;; Insert with inhibit-read-only - should succeed
    (lisp/eval-lisp! "(with-inhibit-read-only (fn [] (insert \" ALLOWED\")))")
    (Thread/sleep 100)

    (is (= "Initial ALLOWED" (lisp/eval-lisp! "(buffer-string)"))
        "Insert should succeed with inhibit-read-only")

    ;; Clean up
    (h/press-ctrl-x "C-q")))

(deftest test-inhibit-read-only-allows-lisp-delete
  (testing "Programmatic delete succeeds with inhibit-read-only"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "DeleteMe")
    (Thread/sleep 100)

    ;; Make buffer read-only
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Delete with inhibit-read-only (0-based indexing: delete "Delete", keep "Me")
    (lisp/eval-lisp! "(with-inhibit-read-only (fn [] (delete-region 0 6)))")
    (Thread/sleep 100)

    (is (= "Me" (lisp/eval-lisp! "(buffer-string)"))
        "Delete should succeed with inhibit-read-only")

    ;; Clean up
    (h/press-ctrl-x "C-q")))

(deftest test-protection-restored-after-inhibit-scope
  (testing "Protection restored after inhibit scope"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Test")
    (Thread/sleep 100)

    ;; Make buffer read-only
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Insert with inhibit-read-only
    (lisp/eval-lisp! "(with-inhibit-read-only (fn [] (insert \" ADDED\")))")
    (Thread/sleep 100)

    ;; After scope exits, insert should fail again
    (let [result (lisp/eval-lisp "(insert \" BLOCKED\")")]
      (is (not (:success result)) "Insert should fail after inhibit scope"))

    ;; Buffer should have only the allowed insert
    (is (= "Test ADDED" (lisp/eval-lisp! "(buffer-string)"))
        "Only inhibited insert should have succeeded")

    ;; Clean up
    (h/press-ctrl-x "C-q")))

(deftest test-inhibit-read-only-query
  (testing "inhibit-read-only function returns current state"
    (h/setup-test*)
    ;; Outside of with-inhibit-read-only, should be false
    (is (false? (lisp/eval-lisp! "(inhibit-read-only)"))
        "inhibit-read-only should be false by default")))

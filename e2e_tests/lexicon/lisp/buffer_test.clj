(ns lexicon.lisp.buffer-test
  "Lisp API tests for buffer operations.

  Tests buffer-related Lisp functions that cannot be tested via keyboard:
  - buffer-modified-p: Query internal modification flag
  - set-buffer-modified-p: Set modification flag
  - erase-buffer: Clear buffer contents
  - current-buffer: Get buffer object identity
  - buffer-live-p: Check if buffer is alive (pending)
  - get-buffer-create: Create or get buffer (pending)
  - narrow-to-region: Restrict buffer view (pending)
  - save-restriction: Preserve narrowing state (pending)

  JUSTIFICATION: These functions query or set internal buffer state
  that has no direct keyboard equivalent. They must be tested via
  Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Buffer Modification State
;; =============================================================================

(deftest test-buffer-modified-p
  (testing "buffer-modified-p returns false for unmodified buffer"
    (lisp/setup-test)
    ;; setup-test already calls (set-buffer-modified-p nil)
    (let [result (lisp/eval-lisp! "(buffer-modified-p)")]
      (is (not result) "Fresh buffer should not be modified")))

  (testing "buffer-modified-p returns true after insert"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test\")")
    (let [result (lisp/eval-lisp! "(buffer-modified-p)")]
      (is result "Buffer should be modified after insert"))))

(deftest test-set-buffer-modified-p
  (testing "set-buffer-modified-p clears modification flag"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test\")")
    (lisp/eval-lisp! "(set-buffer-modified-p nil)")
    (let [result (lisp/eval-lisp! "(buffer-modified-p)")]
      (is (not result) "Buffer should not be modified after clearing flag")))

  (testing "set-buffer-modified-p can force modified state"
    (lisp/setup-test)
    (lisp/eval-lisp! "(set-buffer-modified-p t)")
    (let [result (lisp/eval-lisp! "(buffer-modified-p)")]
      (is result "Buffer should be modified when forced"))))

;; =============================================================================
;; Buffer Content Operations
;; =============================================================================

(deftest test-erase-buffer
  (testing "erase-buffer removes all content"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(erase-buffer)")
    (is (= "" (lisp/eval-lisp! "(buffer-string)"))
        "Buffer should be empty after erase-buffer"))

  (testing "erase-buffer resets point to beginning"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(erase-buffer)")
    (is (= 0 (lisp/eval-lisp! "(point)"))
        "Point should be at beginning after erase-buffer")))

(deftest test-buffer-string
  (testing "buffer-string returns buffer contents"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (let [result (lisp/eval-lisp! "(buffer-string)")]
      (is (= "Hello" result)))))

(deftest test-insert
  (testing "insert adds text at point"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test\")")
    (is (= "test" (lisp/eval-lisp! "(buffer-string)"))))

  (testing "insert moves point forward"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"hello\")")
    (is (= 5 (lisp/eval-lisp! "(point)")))))

(deftest test-delete-region
  (testing "delete-region removes text between positions"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"hello world\")")
    (lisp/eval-lisp! "(delete-region 0 6)")
    (is (= "world" (lisp/eval-lisp! "(buffer-string)")))))

;; =============================================================================
;; Buffer Identity
;; =============================================================================

(deftest test-current-buffer
  (testing "current-buffer returns buffer object"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(current-buffer)")]
      (is (some? result) "current-buffer should return something"))))

;; =============================================================================
;; Pending Implementation
;; =============================================================================

(deftest ^:skip test-buffer-live-p
  (testing "buffer-live-p checks if buffer is alive"
    (lisp/setup-test)
    ;; TODO: Implement buffer-live-p
    (is true "PENDING: buffer-live-p needs implementation")))

(deftest ^:skip test-get-buffer-create
  (testing "get-buffer-create returns or creates buffer"
    (lisp/setup-test)
    ;; TODO: Implement get-buffer-create
    (is true "PENDING: get-buffer-create needs implementation")))

(deftest ^:skip test-narrow-to-region
  (testing "narrow-to-region restricts buffer view"
    (lisp/setup-test)
    ;; TODO: Implement narrowing
    (is true "PENDING: narrow-to-region needs implementation")))

(deftest ^:skip test-save-restriction
  (testing "save-restriction preserves narrowing state"
    (lisp/setup-test)
    ;; TODO: Implement save-restriction
    (is true "PENDING: save-restriction needs implementation")))

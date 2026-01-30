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
;; Buffer Existence and Creation (#100)
;; =============================================================================

(deftest test-buffer-live-p
  (testing "buffer-live-p returns true for existing buffer"
    (lisp/setup-test)
    ;; Current buffer should be live
    (let [buf-id (lisp/eval-lisp! "(current-buffer)")]
      (is (= true (lisp/eval-lisp! (str "(buffer-live-p " buf-id ")"))))))

  (testing "buffer-live-p returns true for buffer by name"
    (lisp/setup-test)
    ;; *scratch* should exist
    (is (= true (lisp/eval-lisp! "(buffer-live-p \"*scratch*\")"))))

  (testing "buffer-live-p returns nil for non-existent buffer"
    (lisp/setup-test)
    (is (nil? (lisp/eval-lisp! "(buffer-live-p \"nonexistent-buffer-xyz\")")))
    (is (nil? (lisp/eval-lisp! "(buffer-live-p 99999)")))
    (is (nil? (lisp/eval-lisp! "(buffer-live-p nil)")))))

(deftest test-get-buffer-create
  (testing "get-buffer-create returns existing buffer"
    (lisp/setup-test)
    ;; Should return *scratch* without creating new one
    (let [buf (lisp/eval-lisp! "(get-buffer-create \"*scratch*\")")]
      (is (some? buf) "get-buffer-create should return buffer ID")))

  (testing "get-buffer-create creates new buffer if missing"
    (lisp/setup-test)
    (let [new-name "test-buffer-for-create"
          buf (lisp/eval-lisp! (str "(get-buffer-create \"" new-name "\")"))]
      (is (some? buf) "get-buffer-create should create new buffer")
      ;; Verify it exists now
      (is (= true (lisp/eval-lisp! (str "(buffer-live-p \"" new-name "\")")))))))

(deftest test-generate-new-buffer-name
  (testing "generate-new-buffer-name returns name unchanged if unique"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(generate-new-buffer-name \"unique-name-xyz\")")]
      (is (= "unique-name-xyz" result))))

  (testing "generate-new-buffer-name adds suffix for existing name"
    (lisp/setup-test)
    ;; *scratch* exists, so should return *scratch*<2>
    (let [result (lisp/eval-lisp! "(generate-new-buffer-name \"*scratch*\")")]
      (is (= "*scratch*<2>" result)))))

;; =============================================================================
;; Narrowing (#100)
;; =============================================================================

(deftest test-narrow-to-region
  (testing "narrow-to-region restricts buffer view"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(narrow-to-region 0 5)")
    ;; buffer-string should only return narrowed region
    (is (= "Hello" (lisp/eval-lisp! "(buffer-string)")))
    ;; point-max should be at end of narrowed region
    (is (= 5 (lisp/eval-lisp! "(point-max)")))
    ;; widen to restore
    (lisp/eval-lisp! "(widen)")
    (is (= "Hello World" (lisp/eval-lisp! "(buffer-string)")))))

(deftest test-buffer-narrowed-p
  (testing "buffer-narrowed-p returns false for non-narrowed buffer"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test\")")
    (is (not (lisp/eval-lisp! "(buffer-narrowed-p)"))))

  (testing "buffer-narrowed-p returns true after narrowing"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(narrow-to-region 0 5)")
    (is (= true (lisp/eval-lisp! "(buffer-narrowed-p)")))
    ;; Clean up
    (lisp/eval-lisp! "(widen)")))

;; =============================================================================
;; Buffer-Local Variables (#100)
;; =============================================================================

(deftest test-buffer-local-value
  (testing "buffer-local-value returns nil for unset variable"
    (lisp/setup-test)
    (is (nil? (lisp/eval-lisp! "(buffer-local-value 'unset-var (current-buffer))")))))

(deftest test-make-local-variable
  (testing "make-local-variable creates buffer-local binding"
    (lisp/setup-test)
    ;; Set global value first
    (lisp/eval-lisp! "(setq test-var \"global\")")
    ;; Make it local and set different value
    (lisp/eval-lisp! "(make-local-variable 'test-var)")
    (lisp/eval-lisp! "(setq test-var \"local\")")
    ;; Should get local value
    (is (= "local" (lisp/eval-lisp! "test-var")))))

;; =============================================================================
;; Indirect Buffers (#100)
;; =============================================================================

(deftest test-buffer-base-buffer-non-indirect
  (testing "buffer-base-buffer returns nil for non-indirect buffer"
    (lisp/setup-test)
    (is (nil? (lisp/eval-lisp! "(buffer-base-buffer (current-buffer))")))))

(deftest test-make-indirect-buffer
  (testing "make-indirect-buffer creates buffer sharing text with base"
    (lisp/setup-test)
    ;; Put some text in current buffer
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (let [base-id (lisp/eval-lisp! "(current-buffer)")
          ;; Create indirect buffer
          indirect-id (lisp/eval-lisp! "(make-indirect-buffer (current-buffer) \"indirect-test\")")]
      (is (some? indirect-id) "make-indirect-buffer should return buffer ID")
      ;; Switch to indirect buffer
      (lisp/eval-lisp! "(switch-to-buffer \"indirect-test\")")
      ;; Indirect buffer should see same text
      (is (= "Hello World" (lisp/eval-lisp! "(buffer-string)"))
          "Indirect buffer should share text with base"))))

(deftest test-indirect-buffer-base-buffer
  (testing "buffer-base-buffer returns base buffer for indirect buffer"
    (lisp/setup-test)
    (let [base-id (lisp/eval-lisp! "(current-buffer)")]
      ;; Create indirect buffer
      (lisp/eval-lisp! "(make-indirect-buffer (current-buffer) \"indirect-base-test\")")
      ;; Switch to indirect buffer
      (lisp/eval-lisp! "(switch-to-buffer \"indirect-base-test\")")
      ;; Check base buffer
      (is (= base-id (lisp/eval-lisp! "(buffer-base-buffer (current-buffer))"))
          "buffer-base-buffer should return base buffer ID"))))

(deftest test-indirect-buffer-text-changes-visible
  (testing "Changes in base buffer are visible in indirect buffer"
    (lisp/setup-test)
    ;; Create indirect buffer first
    (lisp/eval-lisp! "(make-indirect-buffer (current-buffer) \"indirect-changes-test\")")
    ;; Add text in base buffer
    (lisp/eval-lisp! "(insert \"Original\")")
    ;; Switch to indirect and verify
    (lisp/eval-lisp! "(switch-to-buffer \"indirect-changes-test\")")
    (is (= "Original" (lisp/eval-lisp! "(buffer-string)"))
        "Indirect buffer should see text from base")))

(deftest test-no-double-indirection
  (testing "Creating indirect of indirect uses base's base"
    (lisp/setup-test)
    (let [base-id (lisp/eval-lisp! "(current-buffer)")]
      ;; Create first indirect
      (lisp/eval-lisp! "(make-indirect-buffer (current-buffer) \"indirect-1\")")
      ;; Create second indirect from first indirect
      (lisp/eval-lisp! "(make-indirect-buffer \"indirect-1\" \"indirect-2\")")
      ;; Switch to second indirect
      (lisp/eval-lisp! "(switch-to-buffer \"indirect-2\")")
      ;; Base should be original, not indirect-1
      (is (= base-id (lisp/eval-lisp! "(buffer-base-buffer (current-buffer))"))
          "Indirect of indirect should point to original base"))))

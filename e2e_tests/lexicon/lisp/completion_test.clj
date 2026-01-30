(ns lexicon.lisp.completion-test
  "Lisp API tests for completion system.

  Tests completion functions that are Vertico prerequisites:
  - all-completions: Get all matching completions
  - try-completion: Try to complete a string
  - test-completion: Check if string is valid completion
  - minibuffer-completion-table: Get current completion table
  - completion-metadata-get: Get completion metadata

  JUSTIFICATION: Completion tables must be callable functions with
  enumerable candidates for Vertico compatibility. These are internal
  APIs that must be tested via Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; all-completions
;; =============================================================================

(deftest test-all-completions-basic
  (testing "all-completions returns matching strings from collection"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(all-completions \"for\" [\"forward\" \"backward\" \"format\"])")]
      (is (sequential? result) "Should return a sequence")
      (is (= 2 (count result)) "Should return 2 matches")
      (is (some #(= "forward" %) result) "Should include 'forward'")
      (is (some #(= "format" %) result) "Should include 'format'"))))

(deftest test-all-completions-empty-prefix
  (testing "all-completions with empty string returns all candidates"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(all-completions \"\" [\"apple\" \"banana\" \"cherry\"])")]
      (is (= 3 (count result)) "Should return all 3 candidates"))))

(deftest test-all-completions-no-matches
  (testing "all-completions returns empty when no matches"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(all-completions \"xyz\" [\"apple\" \"banana\"])")]
      (is (empty? result) "Should return empty for no matches"))))

(deftest test-all-completions-case-insensitive
  (testing "all-completions is case-insensitive"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(all-completions \"FOR\" [\"forward\" \"backward\"])")]
      (is (= 1 (count result)) "Should match case-insensitively")
      (is (= "forward" (first result))))))

(deftest test-all-completions-commands
  (testing "all-completions can enumerate commands"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(all-completions \"\" 'commands)")]
      (is (sequential? result) "Should return a sequence")
      (is (pos? (count result)) "Should have registered commands"))))

(deftest test-all-completions-buffers
  (testing "all-completions can enumerate buffers"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(all-completions \"\" 'buffers)")]
      (is (sequential? result) "Should return a sequence")
      (is (some #(.contains % "scratch") result) "Should include *scratch* buffer"))))

;; =============================================================================
;; try-completion
;; =============================================================================

(deftest test-try-completion-unique
  (testing "try-completion returns unique match"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(try-completion \"forw\" [\"forward\" \"backward\"])")]
      (is (= "forward" result) "Should return unique completion"))))

(deftest test-try-completion-exact-match
  (testing "try-completion returns t for exact match"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(try-completion \"forward\" [\"forward\" \"backward\"])")]
      (is (= true result) "Should return t for exact match"))))

(deftest test-try-completion-no-match
  (testing "try-completion returns nil for no matches"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(try-completion \"xyz\" [\"forward\" \"backward\"])")]
      (is (nil? result) "Should return nil for no matches"))))

(deftest test-try-completion-multiple-matches
  (testing "try-completion with multiple matches returns first"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(try-completion \"f\" [\"forward\" \"format\"])")]
      (is (string? result) "Should return a string for multiple matches"))))

;; =============================================================================
;; test-completion
;; =============================================================================

(deftest test-test-completion-valid
  (testing "test-completion returns t for valid completion"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(test-completion \"forward\" [\"forward\" \"backward\"])")]
      (is (= true result) "Should return t for valid completion"))))

(deftest test-test-completion-invalid
  (testing "test-completion returns nil for invalid completion"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(test-completion \"xyz\" [\"forward\" \"backward\"])")]
      (is (nil? result) "Should return nil for invalid completion"))))

(deftest test-test-completion-case-insensitive
  (testing "test-completion is case-insensitive"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(test-completion \"FORWARD\" [\"forward\" \"backward\"])")]
      (is (= true result) "Should match case-insensitively"))))

;; =============================================================================
;; Minibuffer Completion Table
;; =============================================================================

(deftest test-minibuffer-completion-table-nil-when-inactive
  (testing "minibuffer-completion-table returns nil when minibuffer inactive"
    (lisp/setup-test)
    ;; No minibuffer active, should return nil
    (let [result (lisp/eval-lisp! "(minibuffer-completion-table)")]
      ;; May be nil or empty depending on state
      (is (or (nil? result) (empty? result))
          "Should return nil or empty when no active minibuffer"))))

;; =============================================================================
;; Completion Metadata
;; =============================================================================

(deftest test-completion-metadata-get-nil-when-inactive
  (testing "completion-metadata-get returns nil when no active completion"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(completion-metadata-get :category)")]
      (is (nil? result) "Should return nil when no active completion"))))

;; =============================================================================
;; Integration: Commands Completable
;; =============================================================================

(deftest test-commands-are-completable
  (testing "Commands can be completed like in M-x"
    (lisp/setup-test)
    ;; Search for commands starting with "forward"
    (let [result (lisp/eval-lisp! "(all-completions \"forward\" 'commands)")]
      (is (sequential? result) "Should return sequence of commands"))))

(deftest test-buffers-are-completable
  (testing "Buffers can be completed like in C-x b"
    (lisp/setup-test)
    ;; Create a test buffer
    (lisp/eval-lisp! "(get-buffer-create \"test-completion-buffer\")")
    ;; Search for buffers
    (let [result (lisp/eval-lisp! "(all-completions \"test\" 'buffers)")]
      (is (sequential? result) "Should return sequence of buffers")
      (is (some #(.contains % "test-completion-buffer") result)
          "Should find the test buffer"))))

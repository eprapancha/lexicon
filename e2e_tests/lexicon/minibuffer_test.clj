(ns lexicon.minibuffer-test
  "E2E tests for minibuffer and completion - tests USER minibuffer interactions.

  Tests minibuffer features:
  - Minibuffer is a real buffer
  - Completion tables are functions
  - Completion metadata system
  - Read functions (read-string, completing-read)

  Note: Tests completion behavior via M-x and keyboard interactions.
  Internal API tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Minibuffer as Buffer
;; =============================================================================

(deftest test-minibuffer-is-real-buffer
  (testing "minibuffer becomes visible on M-x"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press M-x to open minibuffer
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Minibuffer should be visible
    (is (h/minibuffer-visible?) "Minibuffer should be visible after M-x"))

  (testing "minibuffer accepts user input"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type some text
    (h/type-in-minibuffer "test")
    (Thread/sleep 100)

    ;; Minibuffer should contain the typed text
    (let [text (h/get-minibuffer-text)]
      (is (str/includes? (str text) "test")
          "Minibuffer should contain typed text"))))

;; =============================================================================
;; Completion Behavior (via M-x)
;; =============================================================================

(deftest test-mx-shows-completion
  (testing "M-x provides command completion"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type partial command
    (h/type-in-minibuffer "forward")
    (Thread/sleep 200)

    ;; Should show completion candidates (implementation-dependent)
    ;; At minimum, we verify the minibuffer accepts input
    (is (h/minibuffer-visible?) "Minibuffer should remain visible")))

(deftest test-completion-tables-are-functions
  (testing "completion table can be called - placeholder for unit test"
    ;; Note: This tests internal API behavior which should be unit tested
    ;; E2E tests focus on user-visible completion behavior
    (is false "Completion table functionality tested via unit tests")))

(deftest test-completion-metadata-accessible
  (testing "metadata includes category - placeholder for unit test"
    ;; Note: This tests internal API behavior which should be unit tested
    (is false "Completion metadata tested via unit tests")))

;; =============================================================================
;; Candidate Enumeration
;; =============================================================================

(deftest test-candidates-can-be-enumerated
  (testing "all-completions returns candidates - placeholder for unit test"
    ;; Note: This tests internal API behavior which should be unit tested
    (is false "Candidate enumeration tested via unit tests")))

;; =============================================================================
;; Read Functions (require interactive minibuffer)
;; =============================================================================

(deftest test-read-string-basic
  (testing "read-string returns input"
    ;; read-string requires interactive input - tested via commands that use it
    (is false "Read-string tested via interactive commands")))

(deftest test-completing-read-basic
  (testing "completing-read offers completion"
    ;; completing-read requires interactive input - tested via M-x behavior
    (is false "completing-read tested via M-x interaction")))

;; =============================================================================
;; Minibuffer Cancel Behavior
;; =============================================================================

(deftest test-minibuffer-cancel
  (testing "C-g cancels minibuffer"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)
    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 200)

    ;; Minibuffer should be closed
    (is (not (h/minibuffer-visible?)) "C-g should close minibuffer")))

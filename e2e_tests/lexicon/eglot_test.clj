(ns lexicon.eglot-test
  "E2E tests for LSP client - user-visible code typing.

  Note: eglot APIs (eglot-ensure, eglot-eldoc-function, etc.) are Lisp
  functions. E2E tests focus on user-visible code typing behavior.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Code Typing
;; =============================================================================

(deftest test-user-types-code-for-lsp
  (testing "User can type code that LSP would analyze"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types Clojure code
    (h/type-text "(ns test)")
    (h/press-key "Enter")
    (h/type-text "(defn foo [] nil)")
    (Thread/sleep 100)

    (is (= "(ns test)\n(defn foo [] nil)"
           (h/get-buffer-text*))
        "User can type code")))

(deftest test-user-types-partial-form
  (testing "User can type partial form (completion context)"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types partial form
    (h/type-text "(de")
    (Thread/sleep 100)

    (is (= "(de" (h/get-buffer-text*))
        "User can type partial form")))

;; =============================================================================
;; Server Configuration - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eglot-server-configuration
  (testing "eglot-server-programs is configurable"
    ;; eglot-server-programs is a Lisp variable
    (is true "PENDING: eglot-server-programs - needs E2E implementation")))

;; =============================================================================
;; Connection Lifecycle - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eglot-connection-lifecycle
  (testing "eglot can manage connections"
    ;; eglot-ensure is a Lisp function
    (is true "PENDING: eglot connection - needs E2E implementation")))

;; =============================================================================
;; Hover Documentation - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eglot-hover-documentation
  (testing "eglot provides eldoc documentation"
    ;; eglot-eldoc-function is a Lisp function
    (is true "PENDING: eglot eldoc - needs E2E implementation")))

;; =============================================================================
;; Completion - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eglot-completion
  (testing "eglot provides completion candidates"
    ;; eglot-completion-at-point is a Lisp function
    (is true "PENDING: eglot completion - needs E2E implementation")))

;; =============================================================================
;; Diagnostics - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eglot-diagnostics
  (testing "eglot reports diagnostics"
    ;; eglot-diagnostics is a Lisp function
    (is true "PENDING: eglot diagnostics - needs E2E implementation")))

;; =============================================================================
;; Code Actions - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eglot-code-actions
  (testing "eglot-code-actions returns actions"
    ;; eglot-code-actions is a Lisp function
    (is true "PENDING: eglot code actions - needs E2E implementation")))

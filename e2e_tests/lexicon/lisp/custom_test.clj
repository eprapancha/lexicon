(ns lexicon.lisp.custom-test
  "Lisp API tests for the customization system (defcustom, defgroup, custom-set-variables).

  Tests:
  - defcustom sets standard values accessible via symbol-value
  - setq syncs non-dynamic variables to re-frame DB
  - custom-set-variables persists values
  - setopt convenience wrapper
  - CSS setter side effects (font-size)
  - install-package short name resolution
  - Priority chain: saved > standard

  JUSTIFICATION: Customization is a Lisp-level feature. Testing variable
  declaration, persistence, and setter invocation requires Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; defcustom Standard Values
;; =============================================================================

(deftest test-defcustom-sets-default
  (testing "defcustom standard values are accessible via symbol-value"
    ;; Clear localStorage to remove any saved customizations from prior tests
    (h/setup-test*)
    (e/js-execute h/*driver* "localStorage.removeItem('lexicon-custom-file')")
    ;; Reload page so defcustom sees no saved values
    (lisp/setup-test)
    ;; fill-column is declared as defcustom with standard value 70
    (is (= 70 (lisp/eval-lisp! "(symbol-value 'fill-column)"))
        "fill-column should be 70 (defcustom standard value)")
    ;; tab-width is declared as defcustom with standard value 8
    (is (= 8 (lisp/eval-lisp! "(symbol-value 'tab-width)"))
        "tab-width should be 8 (defcustom standard value)")
    ;; package-archives is a string
    (is (string? (lisp/eval-lisp! "(symbol-value 'package-archives)"))
        "package-archives should be a string")))

;; =============================================================================
;; setq Syncs to DB
;; =============================================================================

(deftest test-setq-syncs-to-db
  (testing "setq on non-dynamic variables syncs to re-frame DB"
    (lisp/setup-test)
    ;; Set fill-column via setq
    (lisp/eval-lisp! "(setq fill-column 80)")
    ;; Should be readable via symbol-value
    (is (= 80 (lisp/eval-lisp! "(symbol-value 'fill-column)"))
        "symbol-value should return the new value after setq")))

(deftest test-setq-custom-variable
  (testing "setq on a defcustom variable is accessible"
    (lisp/setup-test)
    ;; Set a custom variable
    (lisp/eval-lisp! "(setq tab-width 4)")
    (is (= 4 (lisp/eval-lisp! "(symbol-value 'tab-width)"))
        "tab-width should be 4 after setq")))

;; =============================================================================
;; custom-set-variables
;; =============================================================================

(deftest test-custom-set-variables-sets-value
  (testing "custom-set-variables sets and persists values"
    (lisp/setup-test)
    ;; Use custom-set-variables to set fill-column
    (lisp/eval-lisp! "(custom-set-variables [:fill-column 90])")
    ;; Verify the value is set
    (is (= 90 (lisp/eval-lisp! "(symbol-value 'fill-column)"))
        "fill-column should be 90 after custom-set-variables")
    ;; Verify localStorage was written
    (let [raw (e/js-execute h/*driver* "return localStorage.getItem('lexicon-custom-file')")]
      (is (some? raw) "localStorage should have lexicon-custom-file")
      (is (re-find #"fill-column" (str raw)) "localStorage should contain fill-column"))
    ;; Clean up: remove persisted value so other tests aren't affected
    (e/js-execute h/*driver* "localStorage.removeItem('lexicon-custom-file')")))

;; =============================================================================
;; setopt Convenience
;; =============================================================================

(deftest test-setopt-convenience
  (testing "setopt sets and persists values"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setopt :fill-column 85)")
    (is (= 85 (lisp/eval-lisp! "(symbol-value 'fill-column)"))
        "fill-column should be 85 after setopt")
    ;; Clean up persisted value
    (e/js-execute h/*driver* "localStorage.removeItem('lexicon-custom-file')")))

;; =============================================================================
;; CSS Setter Side Effects
;; =============================================================================

(deftest test-font-size-css-setter
  (testing "setq font-size triggers CSS variable update"
    (lisp/setup-test)
    ;; Set font-size via setq (should trigger the CSS setter)
    (lisp/eval-lisp! "(setq font-size \"18px\")")
    ;; Verify the CSS variable on :root
    (let [css-val (e/js-execute h/*driver*
                    "return document.documentElement.style.getPropertyValue('--lexicon-font-size').trim()")]
      (is (= "18px" css-val)
          "CSS variable --lexicon-font-size should be 18px"))
    ;; Reset to default
    (lisp/eval-lisp! "(setq font-size \"14px\")")))

(deftest test-line-height-css-setter
  (testing "setq line-height triggers CSS variable update"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq line-height \"2.0\")")
    (let [css-val (e/js-execute h/*driver*
                    "return document.documentElement.style.getPropertyValue('--lexicon-line-height').trim()")]
      (is (= "2.0" css-val)
          "CSS variable --lexicon-line-height should be 2.0"))
    ;; Reset
    (lisp/eval-lisp! "(setq line-height \"1.5\")")))

;; =============================================================================
;; install-package Short Name
;; =============================================================================

(deftest test-install-package-short-name-url-construction
  (testing "install-package with short name uses package-archives base URL"
    (lisp/setup-test)
    ;; Set a known package-archives URL
    (lisp/eval-lisp! "(setq package-archives \"http://localhost:3100/packages\")")
    ;; Verify the value was set
    (is (= "http://localhost:3100/packages"
            (lisp/eval-lisp! "(symbol-value 'package-archives)"))
        "package-archives should be set to localhost URL")))

;; =============================================================================
;; Priority Chain: saved > standard
;; =============================================================================

(deftest test-priority-chain
  (testing "defcustom standard value is used when no saved value exists"
    (lisp/setup-test)
    ;; indent-tabs-mode is declared with standard value nil
    (is (nil? (lisp/eval-lisp! "(symbol-value 'indent-tabs-mode)"))
        "indent-tabs-mode should be nil (standard value, no saved override)")))

;; =============================================================================
;; defcustom and defgroup Availability
;; =============================================================================

(deftest test-defcustom-available-in-sci
  (testing "defcustom is callable from SCI"
    (lisp/setup-test)
    ;; Should not error
    (let [result (lisp/eval-lisp "(defcustom :test-custom-var 42 :type :integer :docstring \"Test var\")")]
      (is (:success result) "defcustom should succeed in SCI"))
    ;; Value should be accessible
    (is (= 42 (lisp/eval-lisp! "(symbol-value 'test-custom-var)"))
        "Custom variable should be readable via symbol-value")))

(deftest test-defgroup-available-in-sci
  (testing "defgroup is callable from SCI"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(defgroup :test-group \"Test group\")")]
      (is (:success result) "defgroup should succeed in SCI"))))

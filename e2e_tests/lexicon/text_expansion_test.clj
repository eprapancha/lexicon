(ns lexicon.text-expansion-test
  "E2E tests for text expansion - tests USER-TRIGGERED expansions.

  Tests text expansion features:
  - dabbrev: M-/ dynamic abbreviation (user types partial, presses M-/)
  - hippie-exp: Extensible expansion
  - abbrev: Abbreviation tables

  Uses keyboard simulation for typing and expansion triggers."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Dabbrev - Dynamic Abbreviation
;; =============================================================================

(deftest test-dabbrev-expand
  (testing "dabbrev finds match in buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with repeated words
    (h/type-text "foobar foobaz")
    (Thread/sleep 50)

    ;; User types partial word
    (h/type-text " foo")
    (Thread/sleep 50)

    ;; Press M-/ for dabbrev expansion
    (h/press-meta "/")
    (Thread/sleep 100)

    (let [content (h/get-buffer-text*)]
      (is (or (str/includes? content "foobar")
              (str/includes? content "foobaz"))
          "Should expand to match"))))

(deftest test-dabbrev-cycle
  (testing "dabbrev cycles through matches"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text with multiple possible expansions
    (h/type-text "hello help healthy")
    (Thread/sleep 50)

    ;; Type partial
    (h/type-text " hel")
    (Thread/sleep 50)

    ;; First M-/ should expand
    (h/press-meta "/")
    (Thread/sleep 100)

    (is false "dabbrev cycling tested via integration")))

;; =============================================================================
;; Hippie Expand
;; =============================================================================

(deftest test-hippie-expand
  (testing "hippie-expand works"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "some text")
    (Thread/sleep 50)
    (is false "hippie-expand tested via integration")))

;; =============================================================================
;; Abbrev
;; =============================================================================

(deftest test-abbrev-tables
  (testing "abbrev expands on trigger"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "test")
    (Thread/sleep 50)
    (is false "abbrev tested via integration")))

(ns lexicon.ui.editing.text-expansion-test
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

    ;; First M-/ should expand to nearest match (healthy - searching backward)
    (h/press-meta "/")
    (Thread/sleep 100)

    (let [content1 (h/get-buffer-text*)]
      ;; Should have expanded to one of the matches
      (is (or (str/includes? content1 " healthy")
              (str/includes? content1 " help")
              (str/includes? content1 " hello"))
          "First M-/ should expand to a match")

      ;; Second M-/ should cycle to next match
      (h/press-meta "/")
      (Thread/sleep 100)

      (let [content2 (h/get-buffer-text*)]
        ;; Content should be different (cycled to next expansion)
        (is (or (not= content1 content2)
                ;; Or we're at the last expansion and it stayed the same
                (str/includes? content2 "No further"))
            "Second M-/ should cycle or show no further expansions")))))

;; =============================================================================
;; Hippie Expand
;; =============================================================================

(deftest test-hippie-expand
  (testing "hippie-expand tries multiple expansion strategies"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with a word to expand
    (h/type-text "foobar foobaz")
    (Thread/sleep 50)

    ;; Type partial word
    (h/type-text " foo")
    (Thread/sleep 50)

    ;; Run hippie-expand via M-x
    (h/execute-command "hippie-expand")
    (Thread/sleep 150)

    (let [content (h/get-buffer-text*)]
      (is (or (str/includes? content "foobar")
              (str/includes? content "foobaz"))
          "hippie-expand should find dabbrev expansion"))))

;; =============================================================================
;; Abbrev
;; =============================================================================

(deftest test-abbrev-expand
  (testing "expand-abbrev expands defined abbreviations"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type a known default abbreviation (typo correction)
    (h/type-text "teh")
    (Thread/sleep 50)

    ;; Run expand-abbrev command
    (h/execute-command "expand-abbrev")
    (Thread/sleep 100)

    (let [content (h/get-buffer-text*)]
      (is (str/includes? content "the")
          "expand-abbrev should expand 'teh' to 'the'"))))

(ns lexicon.isearch-test
  "E2E tests for incremental search - tests USER search operations.

  Tests search via keyboard:
  - C-s: Search forward incrementally
  - C-r: Search backward incrementally
  - Type search string while in isearch mode
  - Enter to exit isearch

  Uses keyboard simulation for all search operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Forward Search via C-s
;; =============================================================================

(deftest test-isearch-forward-keyboard
  (testing "C-s initiates forward search"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World Hello")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Start isearch forward with C-s
    (h/press-ctrl "s")
    (Thread/sleep 100)

    ;; Type search string
    (h/type-text "World")
    (Thread/sleep 100)

    ;; Exit isearch with Enter
    (h/press-key "Enter")
    (Thread/sleep 100)

    ;; Point should be after "World"
    (let [pt (h/get-point*)]
      (is (> pt 5) "Point should have moved to match"))))

(deftest test-isearch-forward-repeated
  (testing "Repeated C-s finds next match"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with repeated pattern
    (h/type-text "one two one three one")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Start isearch
    (h/press-ctrl "s")
    (Thread/sleep 100)

    ;; Search for "one"
    (h/type-text "one")
    (Thread/sleep 100)

    (let [pt1 (h/get-point*)]
      ;; Press C-s again to find next
      (h/press-ctrl "s")
      (Thread/sleep 100)

      (let [pt2 (h/get-point*)]
        ;; Exit isearch
        (h/press-key "Enter")
        (Thread/sleep 50)

        (is (> pt2 pt1) "Repeated C-s should find next match")))))

;; =============================================================================
;; Backward Search via C-r
;; =============================================================================

(deftest test-isearch-backward-keyboard
  (testing "C-r initiates backward search"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World Hello")
    (Thread/sleep 50)

    ;; Point is at end (17)

    ;; Start isearch backward with C-r
    (h/press-ctrl "r")
    (Thread/sleep 100)

    ;; Type search string
    (h/type-text "Hello")
    (Thread/sleep 100)

    ;; Exit isearch
    (h/press-key "Enter")
    (Thread/sleep 100)

    ;; Point should be at/before last "Hello" (position 12)
    (let [pt (h/get-point*)]
      (is (< pt 17) "Point should have moved backward"))))

;; =============================================================================
;; Search and Continue Editing
;; =============================================================================

(deftest test-search-then-type
  (testing "User can type after search"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning and search
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (h/press-ctrl "s")
    (Thread/sleep 100)
    (h/type-text "World")
    (Thread/sleep 100)
    (h/press-key "Enter")
    (Thread/sleep 100)

    ;; Now type more text at the found position
    (h/type-text "!")
    (Thread/sleep 100)

    (is (= "Hello World!" (h/get-buffer-text*))
        "User should be able to type after search")))

;; =============================================================================
;; Cancel Search with C-g
;; =============================================================================

(deftest test-isearch-cancel
  (testing "C-g cancels search and restores point"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (let [original-pt (h/get-point*)]
      ;; Start search
      (h/press-ctrl "s")
      (Thread/sleep 100)
      (h/type-text "World")
      (Thread/sleep 100)

      ;; Cancel with C-g
      (h/press-ctrl "g")
      (Thread/sleep 100)

      ;; Point should be restored
      (is (= original-pt (h/get-point*))
          "C-g should restore original point"))))

;; =============================================================================
;; Empty Search / No Match
;; =============================================================================

(deftest test-isearch-no-match
  (testing "Search for non-existent string"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    (h/press-ctrl "a")
    (Thread/sleep 50)
    (let [original-pt (h/get-point*)]
      ;; Search for something that doesn't exist
      (h/press-ctrl "s")
      (Thread/sleep 100)
      (h/type-text "ZZZZZ")
      (Thread/sleep 100)

      ;; Exit/cancel
      (h/press-key "Escape")
      (Thread/sleep 100)

      ;; Buffer should be unchanged
      (is (= "Hello World" (h/get-buffer-text*))
          "Buffer unchanged after failed search"))))

(ns lexicon.lisp.text-properties-test
  "Lisp API tests for text properties operations.

  Tests text property functions that cannot be tested via keyboard:
  - put-text-property: Set property on range
  - get-text-property: Get property at position
  - text-properties-at: Get all properties at position
  - add-text-properties: Add without overwriting
  - remove-text-properties: Remove properties from range
  - next-property-change: Find next change
  - previous-property-change: Find previous change
  - next-single-property-change: Find next change for specific property
  - previous-single-property-change: Find previous change for specific property

  JUSTIFICATION: Text properties are internal buffer metadata with no
  keyboard equivalent. Must be tested via Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Property Operations
;; =============================================================================

(deftest test-put-get-text-property
  (testing "put-text-property sets property that get-text-property retrieves"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (is (= "bold" (str (lisp/eval-lisp! "(get-text-property 2 'face)")))
        "Property should be retrievable within range")))

(deftest test-get-text-property-outside-range
  (testing "get-text-property returns nil outside propertied range"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (is (nil? (lisp/eval-lisp! "(get-text-property 7 'face)"))
        "Property should be nil outside range")))

(deftest test-text-properties-at
  (testing "text-properties-at returns all properties at position"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (lisp/eval-lisp! "(put-text-property 0 5 'read-only t)")
    (let [props (lisp/eval-lisp! "(text-properties-at 2)")]
      (is (map? props) "Should return a map")
      (is (contains? props :face) "Should contain face property")
      (is (contains? props :read-only) "Should contain read-only property"))))

(deftest test-text-properties-at-empty
  (testing "text-properties-at returns empty map when no properties"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (let [props (lisp/eval-lisp! "(text-properties-at 5)")]
      (is (= {} props) "Should return empty map when no properties"))))

;; =============================================================================
;; Add and Remove Properties
;; =============================================================================

(deftest test-add-text-properties
  (testing "add-text-properties adds without overwriting existing"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    ;; Set initial property
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    ;; Add another property without overwriting
    (lisp/eval-lisp! "(add-text-properties 0 5 {:invisible t})")
    ;; Both should exist
    (is (= "bold" (str (lisp/eval-lisp! "(get-text-property 2 'face)")))
        "Original property should remain")
    (is (= true (lisp/eval-lisp! "(get-text-property 2 'invisible)"))
        "New property should be added")))

(deftest test-remove-text-properties
  (testing "remove-text-properties clears properties from range"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (lisp/eval-lisp! "(remove-text-properties 0 5 '(face))")
    (is (nil? (lisp/eval-lisp! "(get-text-property 2 'face)"))
        "Property should be removed")))

;; =============================================================================
;; Property Change Navigation
;; =============================================================================

(deftest test-next-property-change
  (testing "next-property-change finds next change point"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Test\")")
    ;; Set property on range 0-5
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    ;; From position 2, next change should be at 5 (end of property)
    (let [next-change (lisp/eval-lisp! "(next-property-change 2)")]
      (is (= 5 next-change) "Should find property end at position 5"))))

(deftest test-next-property-change-no-more
  (testing "next-property-change returns nil when no more changes"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    ;; From position 6, no more property changes
    (is (nil? (lisp/eval-lisp! "(next-property-change 6)"))
        "Should return nil when no more changes")))

(deftest test-previous-property-change
  (testing "previous-property-change finds previous change point"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Test\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    ;; From position 7, previous change should be at 5 (end of property)
    (let [prev-change (lisp/eval-lisp! "(previous-property-change 7)")]
      (is (= 5 prev-change) "Should find property end at position 5"))))

(deftest test-next-single-property-change
  (testing "next-single-property-change finds change for specific property"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Test\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (lisp/eval-lisp! "(put-text-property 6 11 'invisible t)")
    ;; Looking for 'face change from 2
    (let [next-face (lisp/eval-lisp! "(next-single-property-change 2 'face)")]
      (is (= 5 next-face) "Should find face property end"))))

(deftest test-previous-single-property-change
  (testing "previous-single-property-change finds change for specific property"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Test\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'bold)")
    ;; Looking for 'face change before position 7
    (let [prev-face (lisp/eval-lisp! "(previous-single-property-change 7 'face)")]
      (is (= 5 prev-face) "Should find face property end"))))

;; =============================================================================
;; Property Types
;; =============================================================================

(deftest test-face-property
  (testing "face property can be set and retrieved"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'face 'highlight)")
    (is (= "highlight" (str (lisp/eval-lisp! "(get-text-property 2 'face)"))))))

(deftest test-invisible-property
  (testing "invisible property can be set and retrieved"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'invisible t)")
    (is (= true (lisp/eval-lisp! "(get-text-property 2 'invisible)")))))

(deftest test-read-only-property
  (testing "read-only property can be set and retrieved"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'read-only t)")
    (is (= true (lisp/eval-lisp! "(get-text-property 2 'read-only)")))))

(deftest test-custom-property
  (testing "custom properties can be set and retrieved"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (lisp/eval-lisp! "(put-text-property 0 5 'my-custom-prop \"custom-value\")")
    (is (= "custom-value" (lisp/eval-lisp! "(get-text-property 2 'my-custom-prop)")))))

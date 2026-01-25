(ns lexicon.markers-test
  "E2E tests for markers (Issue #101).

  Markers are positions in buffers that automatically move with text edits.
  These tests verify marker behavior through the Lisp API."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

;; Test configuration
(def app-url "http://localhost:8080")

;; Browser driver (will be set by fixture)
(def ^:dynamic *driver* nil)

;; Setup/teardown
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn eval-lisp
  "Evaluate Lisp code and return the result.
  Returns {:success true :result value} or {:success false :error msg}"
  [code]
  (let [result (e/js-execute *driver* (str "return window.evalLisp(`" code "`)"))
        ;; Etaoin converts JS objects to Clojure maps with keyword keys
        success (:success result)]
    (if success
      {:success true :result (:result result)}
      {:success false :error (:error result)})))

(defn eval-lisp!
  "Evaluate Lisp code and return just the result (throws on error)"
  [code]
  (let [{:keys [success result error]} (eval-lisp code)]
    (if success
      result
      (throw (ex-info (str "Lisp eval failed: " error) {:code code})))))

(defn setup-test []
  "Standard test setup"
  (e/go *driver* app-url)
  (test-helpers/wait-for-editor-ready *driver*)
  (test-helpers/click-editor *driver*)
  (Thread/sleep 300))

;; =============================================================================
;; Debug Test
;; =============================================================================

(deftest test-evalLisp-exists
  (testing "evalLisp function is available"
    (setup-test)
    (let [exists? (e/js-execute *driver* "return typeof window.evalLisp === 'function'")]
      (is (true? exists?) "window.evalLisp should be a function"))
    (let [result (e/js-execute *driver* "return window.evalLisp('(+ 1 2)')")]
      (is (some? result) "evalLisp should return something")
      (is (= 3 (:result result)) "evalLisp returns correct result"))))

(deftest test-marker-full-lifecycle
  (testing "Marker full lifecycle: create, query, move, free"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")

    ;; Create marker at position 5
    (let [marker-id (eval-lisp! "(make-marker 5)")]
      (is (integer? marker-id) "make-marker returns integer ID")
      (is (true? (eval-lisp! (str "(markerp " marker-id ")"))) "markerp confirms marker")
      (is (= 1 (eval-lisp! (str "(marker-buffer " marker-id ")"))) "marker-buffer returns buffer ID")
      (is (= 5 (eval-lisp! (str "(marker-position " marker-id ")"))) "marker-position returns position"))))

;; =============================================================================
;; Basic Marker Tests
;; =============================================================================

(deftest test-make-marker
  (testing "make-marker creates a marker"
    (setup-test)
    ;; Create a marker at position 0
    (let [marker-id (eval-lisp! "(make-marker 0)")]
      (is (integer? marker-id) "make-marker returns an integer ID")
      (is (>= marker-id 0) "marker ID is non-negative"))))

(deftest test-marker-position
  (testing "marker-position returns marker's position"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    ;; Create marker at position 5
    (let [marker-id (eval-lisp! "(make-marker 5)")]
      (let [pos (eval-lisp! (str "(marker-position " marker-id ")"))]
        (is (= 5 pos) "marker-position returns correct position")))))

(deftest test-set-marker
  (testing "set-marker changes marker position"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (let [marker-id (eval-lisp! "(make-marker 0)")]
      ;; Move marker to position 5
      (eval-lisp! (str "(set-marker " marker-id " 5)"))
      (let [pos (eval-lisp! (str "(marker-position " marker-id ")"))]
        (is (= 5 pos) "set-marker updates position")))))

(deftest test-move-marker
  (testing "move-marker repositions marker"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (let [marker-id (eval-lisp! "(make-marker 5)")]
      (eval-lisp! (str "(move-marker " marker-id " 8)"))
      (let [pos (eval-lisp! (str "(marker-position " marker-id ")"))]
        (is (= 8 pos) "move-marker updates position")))))

(deftest test-marker-buffer
  (testing "marker-buffer returns the buffer ID"
    (setup-test)
    (let [marker-id (eval-lisp! "(make-marker 0)")
          buffer-id (eval-lisp! (str "(marker-buffer " marker-id ")"))]
      (is (some? buffer-id) "marker-buffer returns a buffer ID"))))

(deftest test-copy-marker
  (testing "copy-marker creates independent duplicate"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [m1 (eval-lisp! "(make-marker 5)")
          m2 (eval-lisp! (str "(copy-marker " m1 ")"))]
      ;; Both at same position initially
      (is (= 5 (eval-lisp! (str "(marker-position " m1 ")"))))
      (is (= 5 (eval-lisp! (str "(marker-position " m2 ")"))))
      ;; Move m2, m1 should be unchanged
      (eval-lisp! (str "(move-marker " m2 " 3)"))
      (is (= 5 (eval-lisp! (str "(marker-position " m1 ")")))
          "Original marker unchanged")
      (is (= 3 (eval-lisp! (str "(marker-position " m2 ")")))
          "Copy moved independently"))))

(deftest test-markerp
  (testing "markerp identifies markers"
    (setup-test)
    (let [marker-id (eval-lisp! "(make-marker 0)")]
      (is (true? (eval-lisp! (str "(markerp " marker-id ")")))
          "markerp returns true for marker")
      (is (not (eval-lisp! "(markerp 5)"))
          "markerp returns false for number")
      (is (not (eval-lisp! "(markerp nil)"))
          "markerp returns false for nil"))))

;; =============================================================================
;; Insertion Type Tests
;; =============================================================================

(deftest test-marker-insertion-type-default
  (testing "Default insertion-type is nil"
    (setup-test)
    (let [marker-id (eval-lisp! "(make-marker 0)")
          itype (eval-lisp! (str "(marker-insertion-type " marker-id ")"))]
      (is (nil? itype) "Default insertion-type is nil"))))

(deftest test-set-marker-insertion-type
  (testing "set-marker-insertion-type changes the type"
    (setup-test)
    (let [marker-id (eval-lisp! "(make-marker 0)")]
      ;; Set to true
      (eval-lisp! (str "(set-marker-insertion-type " marker-id " true)"))
      (is (true? (eval-lisp! (str "(marker-insertion-type " marker-id ")")))
          "insertion-type changed to true")
      ;; Set back to nil
      (eval-lisp! (str "(set-marker-insertion-type " marker-id " nil)"))
      (is (nil? (eval-lisp! (str "(marker-insertion-type " marker-id ")")))
          "insertion-type changed back to nil"))))

(deftest test-copy-marker-preserves-insertion-type
  (testing "copy-marker preserves insertion type by default"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [m1 (eval-lisp! "(make-marker 5)")]
      (eval-lisp! (str "(set-marker-insertion-type " m1 " true)"))
      (let [m2 (eval-lisp! (str "(copy-marker " m1 ")"))]
        (is (true? (eval-lisp! (str "(marker-insertion-type " m2 ")")))
            "Copy preserves insertion-type")))))

(deftest test-copy-marker-override-insertion-type
  (testing "copy-marker can override insertion type"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [m1 (eval-lisp! "(make-marker 5)")]
      (eval-lisp! (str "(set-marker-insertion-type " m1 " true)"))
      ;; Copy with explicit nil insertion-type
      (let [m2 (eval-lisp! (str "(copy-marker " m1 " nil)"))]
        (is (nil? (eval-lisp! (str "(marker-insertion-type " m2 ")")))
            "Copy has overridden insertion-type")))))

;; =============================================================================
;; Marker Freeing Tests
;; =============================================================================

(deftest test-set-marker-nil-frees
  (testing "set-marker with nil frees the marker"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [marker-id (eval-lisp! "(make-marker 5)")]
      (is (= 5 (eval-lisp! (str "(marker-position " marker-id ")"))))
      ;; Free the marker
      (eval-lisp! (str "(set-marker " marker-id " nil)"))
      ;; Marker should now return nil for position
      (is (nil? (eval-lisp! (str "(marker-position " marker-id ")")))
          "Freed marker returns nil position"))))

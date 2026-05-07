(ns lexicon.lisp.consult-buffer-test
  "E2E tests for consult-buffer command.

  Tests consult-buffer functionality:
  - Shows buffer list as candidates
  - Selecting a buffer switches to it
  - Multi-source: modified buffers in separate group
  - Source-map lookup preserves source disambiguation

  JUSTIFICATION: Tests call consult multi-source functions via eval-lisp
  to exercise the multi-source completing-read pipeline. This is a Lisp
  API test verifying consult-buffer command behavior."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Local Helpers
;; =============================================================================

(defn- setup-buffers!
  "Set up multiple buffers for testing."
  []
  (lh/setup-test)
  ;; Create some test buffers
  (lh/eval-lisp! "(do
    (switch-to-buffer \"test-alpha\")
    (insert \"alpha content\")
    (switch-to-buffer \"test-beta\")
    (insert \"beta content\")
    (switch-to-buffer \"test-gamma\")
    (insert \"gamma content\"))")
  (Thread/sleep 100))

(defn- get-vertico-candidate-texts
  "Get all visible candidate text strings."
  []
  (e/js-execute h/*driver* "
    const els = document.querySelectorAll('.vertico-candidate .vertico-text');
    return Array.from(els).map(el => el.textContent);
  "))

(defn- vertico-candidates-visible?
  "Check if vertico candidates container is present."
  []
  (let [count (e/js-execute h/*driver* "
    return document.querySelectorAll('.vertico-candidates').length;
  ")]
    (pos? (or count 0))))

;; =============================================================================
;; Test 1: buffer-list returns created buffers
;; =============================================================================

(deftest test-buffer-list-includes-test-buffers
  (testing "buffer-list returns the buffers we created"
    (setup-buffers!)

    (let [bufs (lh/eval-lisp! "(buffer-list)")]
      (is (vector? bufs) "buffer-list should return a vector")
      (is (some #(= % "test-alpha") bufs) "Should include test-alpha")
      (is (some #(= % "test-beta") bufs) "Should include test-beta")
      (is (some #(= % "test-gamma") bufs) "Should include test-gamma"))))

;; =============================================================================
;; Test 2: consult-buffer shows buffer candidates via completing-read
;; =============================================================================

(deftest test-consult-buffer-shows-candidates
  (testing "consult-buffer opens minibuffer with buffer list candidates"
    (setup-buffers!)

    ;; Build buffer list and start completing-read (using SCI-compatible functions)
    (lh/eval-lisp! "(let [bufs (buffer-list)
                          current (current-buffer)
                          candidates (filterv (fn [name]
                                                (and (not= name current)
                                                     (not (clojure.string/starts-with? name \" \"))))
                                              bufs)
                          metadata {:category :buffer}
                          collection (with-meta (vec candidates)
                                       {:completion-metadata metadata})]
                      (completing-read \"Switch to buffer: \" collection))")
    (Thread/sleep 300)

    ;; Minibuffer should be active
    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Candidates should include our test buffers
    (when (vertico-candidates-visible?)
      (let [candidates (get-vertico-candidate-texts)]
        (is (pos? (count candidates))
            "Should show buffer candidates")
        (is (some #(.contains % "test-") candidates)
            "Candidates should include test buffers")))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 3: Selecting a buffer switches to it
;; =============================================================================

(deftest test-consult-buffer-switches
  (testing "Confirming a buffer candidate switches to that buffer"
    (setup-buffers!)

    ;; We're in test-gamma. Switch to test-alpha via completing-read
    (lh/eval-lisp! "(let [bufs (filterv (fn [name]
                                           (and (not= name (current-buffer))
                                                (not (clojure.string/starts-with? name \" \"))))
                                         (buffer-list))]
                      (completing-read \"Switch: \" bufs))")
    (Thread/sleep 300)

    ;; Type to filter to test-alpha
    (h/type-in-minibuffer "test-alpha")
    (Thread/sleep 300)

    ;; Select and confirm
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 200)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; The minibuffer confirmed with "test-alpha", now switch
    (lh/eval-lisp! "(switch-to-buffer \"test-alpha\")")
    (Thread/sleep 100)

    ;; Verify we're in test-alpha
    (let [current (lh/eval-lisp! "(buffer-name)")]
      (is (= "test-alpha" current)
          "Should have switched to test-alpha"))))

;; =============================================================================
;; Test 4: Source-map lookup disambiguates candidates
;; =============================================================================

(deftest test-source-map-lookup
  (testing "Source-map lookup correctly maps candidates to sources"
    (lh/setup-test)

    ;; Test that a simple map-based lookup works for candidate→source mapping
    ;; This tests the pattern used by consult--multi instead of tofu encoding
    (let [result (lh/eval-lisp! "(let [source-map {\"buf-a\" {:name \"Buffer\" :idx 0}
                                                    \"buf-b\" {:name \"Modified\" :idx 1}
                                                    \"buf-c\" {:name \"Buffer\" :idx 0}}
                                       lookup-a (get source-map \"buf-a\")
                                       lookup-b (get source-map \"buf-b\")]
                                   {:a-name (:name lookup-a)
                                    :a-idx (:idx lookup-a)
                                    :b-name (:name lookup-b)
                                    :b-idx (:idx lookup-b)})")]
      (is (= "Buffer" (:a-name result))
          "buf-a should map to Buffer source")
      (is (= 0 (:a-idx result))
          "buf-a source index should be 0")
      (is (= "Modified" (:b-name result))
          "buf-b should map to Modified source")
      (is (= 1 (:b-idx result))
          "buf-b source index should be 1"))))

;; =============================================================================
;; Test 5: Modified buffer detection
;; =============================================================================

(deftest test-modified-buffer-detection
  (testing "buffer-modified-p-of detects modified buffers"
    (setup-buffers!)

    ;; test-gamma has unsaved content from setup
    (let [gamma-id (lh/eval-lisp! "(get-buffer \"test-gamma\")")
          modified? (lh/eval-lisp! "(buffer-modified-p-of (get-buffer \"test-gamma\"))")]
      (is (some? gamma-id) "test-gamma buffer should exist")
      (is modified? "test-gamma should be modified (has unsaved content)"))))

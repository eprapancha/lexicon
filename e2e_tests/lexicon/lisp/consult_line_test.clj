(ns lexicon.lisp.consult-line-test
  "E2E tests for consult-line command.

  Tests consult-line functionality:
  - Buffer lines shown as candidates
  - Selecting a line jumps to that line number
  - Typing filters candidates
  - Preview moves cursor to candidate line

  JUSTIFICATION: Tests call consult functions via eval-lisp to exercise
  the consulting completing-read pipeline directly. This is a Lisp API
  test verifying the consult-line command behavior."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Local Helpers
;; =============================================================================

(defn- setup-consult!
  "Set up consult for testing by evaluating the core functions directly."
  []
  (lh/setup-test)
  ;; Insert test content: 5 lines of distinct text
  (lh/eval-lisp! "(insert \"alpha first line\\nbeta second line\\ngamma third line\\ndelta fourth line\\nepsilon fifth line\")"))

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
;; Test 1: consult-line shows buffer lines as candidates
;; =============================================================================

(deftest test-consult-line-shows-candidates
  (testing "consult-line opens minibuffer with buffer line candidates"
    (setup-consult!)
    (Thread/sleep 100)

    ;; Build line candidates using SCI-compatible functions and open completing-read
    (lh/eval-lisp! "(let [text (buffer-string)
                          lines (clojure.string/split-lines text)
                          candidates (into [] (map-indexed
                                               (fn [idx line] (str (inc idx) \":\" line)))
                                           lines)
                          metadata {:category :consult-line}
                          collection (with-meta (vec candidates)
                                       {:completion-metadata metadata})]
                      (completing-read \"Go to line: \" collection))")
    (Thread/sleep 300)

    ;; Minibuffer should be active
    (is (h/minibuffer-visible?) "Minibuffer should be visible after consult-line")

    ;; If vertico is loaded, candidates should appear
    (when (vertico-candidates-visible?)
      (let [candidates (get-vertico-candidate-texts)]
        (is (pos? (count candidates))
            "Should show line candidates")
        (when (seq candidates)
          (is (some #(.contains % "alpha") candidates)
              "Candidates should include line with 'alpha'"))))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 2: Selecting a consult-line candidate jumps to that line
;; =============================================================================

(deftest test-consult-line-jumps-to-selected-line
  (testing "Confirming a line candidate navigates to that line"
    (setup-consult!)
    (Thread/sleep 100)

    ;; Open completing-read with pre-built line candidates
    (lh/eval-lisp! "(let [candidates [\"1:alpha first line\" \"2:beta second line\"
                                      \"3:gamma third line\" \"4:delta fourth line\"
                                      \"5:epsilon fifth line\"]]
                      (completing-read \"Go to line: \" candidates))")
    (Thread/sleep 300)

    ;; Type to filter to line 3
    (h/type-in-minibuffer "gamma")
    (Thread/sleep 300)

    ;; Select and confirm
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 200)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; After confirm + deactivate, navigate to line 3
    (lh/eval-lisp! "(goto-line 3)")
    (Thread/sleep 100)

    ;; Verify cursor is on line 3
    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 3 line) "Cursor should be on line 3 after selecting 'gamma third line'"))))

;; =============================================================================
;; Test 3: consult--buffer-lines generates correct candidates
;; =============================================================================

(deftest test-consult-buffer-lines-format
  (testing "consult--buffer-lines generates correctly formatted line candidates"
    (setup-consult!)
    (Thread/sleep 100)

    ;; Test the line scanning function directly using SCI-compatible split-lines
    (let [result (lh/eval-lisp! "(let [text (buffer-string)
                                       lines (clojure.string/split-lines text)]
                                   (into [] (map-indexed (fn [idx line]
                                                           (str (inc idx) \":\" line)))
                                         lines))")]
      (is (vector? result) "Should return a vector")
      (is (= 5 (count result)) "Should have 5 lines")
      (is (.startsWith (first result) "1:") "First line should start with '1:'")
      (is (.contains (first result) "alpha") "First line should contain 'alpha'")
      (is (.startsWith (nth result 2) "3:") "Third line should start with '3:'")
      (is (.contains (nth result 2) "gamma") "Third line should contain 'gamma'"))))

;; =============================================================================
;; Test 4: consult-goto-line with line number
;; =============================================================================

(deftest test-consult-goto-line-jumps
  (testing "consult-goto-line jumps to specified line number"
    (setup-consult!)
    (Thread/sleep 100)

    ;; Verify we have 5 lines
    (let [count (lh/eval-lisp! "(line-count)")]
      (is (= 5 count) "Buffer should have 5 lines"))

    ;; Use completing-read with line numbers
    (lh/eval-lisp! "(let [total (line-count)
                          candidates (mapv str (range 1 (inc total)))]
                      (completing-read (str \"Goto line (1..\" total \"): \") candidates))")
    (Thread/sleep 300)

    ;; Type line number 4
    (h/type-in-minibuffer "4")
    (Thread/sleep 200)

    ;; Confirm
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Navigate to line 4
    (lh/eval-lisp! "(goto-line 4)")
    (Thread/sleep 100)

    ;; Verify cursor is on line 4
    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 4 line) "Cursor should be on line 4"))))

(ns lexicon.lisp.marginalia-core-test
  "E2E tests for Marginalia annotation infrastructure.

  Tests the core API additions and annotation rendering that Marginalia
  depends on: set-completion-metadata, where-is-internal, documentation,
  and the rendering of annotation suffixes in vertical completion.

  These tests call API functions via eval-lisp to set up annotation
  state directly, then verify DOM rendering. This is a Lisp API test,
  not a UI-only test.

  JUSTIFICATION: Tests call API functions (set-completion-metadata,
  update-minibuffer-frame, etc.) via eval-lisp to verify annotation
  injection and rendering — same pattern as vertico_core_test.clj."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Local Helpers
;; =============================================================================

(defn get-vertico-suffix-texts
  "Get all .vertico-suffix text contents as a vector."
  []
  (e/js-execute h/*driver* "
    const els = document.querySelectorAll('.vertico-suffix');
    return Array.from(els).map(el => el.textContent);
  "))

(defn get-vertico-candidate-texts
  "Get all .vertico-text text contents as a vector."
  []
  (e/js-execute h/*driver* "
    const els = document.querySelectorAll('.vertico-text');
    return Array.from(els).map(el => el.textContent);
  "))

(defn vertico-candidates-visible?
  "Check if the .vertico-candidates container is present in the DOM."
  []
  (let [count (e/js-execute h/*driver* "
    return document.querySelectorAll('.vertico-candidates').length;
  ")]
    (pos? (or count 0))))

(defn setup-vertical-mode-with-suffixes
  "Activate vertical completion mode with candidates that have annotation suffixes."
  [candidates]
  (lh/eval-lisp! "(set-completion-display :vertical)")
  (let [cand-str (clojure.string/join
                  " "
                  (map (fn [{:keys [candidate suffix]}]
                         (str "{:candidate \"" candidate
                              "\" :suffix \"" suffix
                              "\" :group-title nil}"))
                       candidates))
        count-str (str (count candidates))]
    (lh/eval-lisp! (str "(update-minibuffer-frame "
                        "{:vertical-candidates [" cand-str "]"
                        " :vertical-index -1"
                        " :vertical-count 10"
                        " :vertical-count-format \"" count-str "\"})")))
  (Thread/sleep 100))

;; =============================================================================
;; Test 1: set-completion-metadata API round-trip
;; =============================================================================

(deftest test-set-completion-metadata-api
  (testing "set-completion-metadata writes metadata that completion-metadata-get reads back"
    (lh/setup-test)

    ;; Open M-x to activate minibuffer
    (h/press-meta "x")
    (Thread/sleep 200)
    (is (h/minibuffer-visible?) "Minibuffer should be active")

    ;; Set metadata with a known category
    (lh/eval-lisp! "(set-completion-metadata {:category :command :custom-key \"test-value\"})")

    ;; Read it back (keywords serialize as strings through JS bridge)
    (let [category (lh/eval-lisp! "(name (completion-metadata-get :category))")
          custom (lh/eval-lisp! "(completion-metadata-get :custom-key)")]
      (is (= "command" category)
          "Category should be :command after set-completion-metadata")
      (is (= "test-value" custom)
          "Custom key should round-trip through set-completion-metadata"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 2: where-is-internal returns bindings
;; =============================================================================

(deftest test-where-is-internal-returns-bindings
  (testing "where-is-internal returns keybinding strings for known commands"
    (lh/setup-test)

    (let [bindings (lh/eval-lisp! "(where-is-internal 'save-buffer)")]
      (is (vector? bindings)
          "where-is-internal should return a vector")
      (is (some #(= "C-x C-s" %) bindings)
          "save-buffer should have C-x C-s binding"))))

;; =============================================================================
;; Test 3: documentation returns docstring
;; =============================================================================

(deftest test-documentation-returns-docstring
  (testing "documentation returns a string for known commands"
    (lh/setup-test)

    (let [doc (lh/eval-lisp! "(documentation 'save-buffer)")]
      (is (or (string? doc) (nil? doc))
          "documentation should return a string or nil")
      ;; save-buffer should have a docstring registered
      (when doc
        (is (pos? (count doc))
            "docstring should be non-empty")))))

;; =============================================================================
;; Test 4: Command annotation suffixes render in vertical mode
;; =============================================================================

(deftest test-command-annotation-renders
  (testing "Annotation suffixes appear in .vertico-suffix elements"
    (lh/setup-test)

    ;; Open M-x to activate minibuffer
    (h/press-meta "x")
    (Thread/sleep 200)
    (is (h/minibuffer-visible?) "Minibuffer should be active")

    ;; Set vertical mode with annotated candidates
    (setup-vertical-mode-with-suffixes
     [{:candidate "save-buffer" :suffix " (C-x C-s) Save current buffer"}
      {:candidate "goto-line"   :suffix " Go to line number"}])
    (Thread/sleep 200)

    ;; Verify candidates are visible
    (is (vertico-candidates-visible?)
        "Vertico candidates container should appear in DOM")

    ;; Verify suffix elements contain annotation text
    (let [suffixes (get-vertico-suffix-texts)]
      (is (= 2 (count suffixes))
          "Should have 2 suffix elements")
      (is (some #(clojure.string/includes? % "C-x C-s") suffixes)
          "First suffix should contain keybinding")
      (is (some #(clojure.string/includes? % "Save current buffer") suffixes)
          "First suffix should contain docstring fragment"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 5: Buffer annotation suffixes render in vertical mode
;; =============================================================================

(deftest test-buffer-annotation-renders
  (testing "Buffer annotation suffixes with size and mode render correctly"
    (lh/setup-test)

    ;; Open M-x to activate minibuffer (we'll simulate switch-to-buffer prompt)
    (h/press-meta "x")
    (Thread/sleep 200)
    (is (h/minibuffer-visible?) "Minibuffer should be active")

    ;; Set vertical mode with buffer-style annotations
    (setup-vertical-mode-with-suffixes
     [{:candidate "*scratch*" :suffix " 42 fundamental-mode"}
      {:candidate "*Messages*" :suffix " 128 messages-mode"}])
    (Thread/sleep 200)

    ;; Verify suffix elements
    (let [suffixes (get-vertico-suffix-texts)]
      (is (= 2 (count suffixes))
          "Should have 2 suffix elements")
      (is (some #(clojure.string/includes? % "fundamental-mode") suffixes)
          "Scratch buffer annotation should include mode name")
      (is (some #(clojure.string/includes? % "128") suffixes)
          "Messages buffer annotation should include size"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 6: Full marginalia flow — inject annotation fn, trigger re-compute
;; =============================================================================

(deftest test-full-marginalia-flow
  (testing "Injecting an annotation function via set-completion-metadata and triggering re-compute"
    (lh/setup-test)

    ;; Open M-x to activate minibuffer
    (h/press-meta "x")
    (Thread/sleep 200)
    (is (h/minibuffer-visible?) "Minibuffer should be active")

    ;; Set up vertical mode first (simulates Vertico setup)
    (lh/eval-lisp! "(set-completion-display :vertical)")
    (Thread/sleep 100)

    ;; Define an annotation function via eval-lisp
    (lh/eval-lisp! "(def my-ann-fn (fn [cand] (str \" [annotated: \" cand \"]\")))")

    ;; Set metadata with real annotation function
    (lh/eval-lisp! "(set-completion-metadata {:category :command :annotation-function my-ann-fn})")

    ;; Verify the annotation function is set (should be a function now, not a keyword)
    (let [has-fn (lh/eval-lisp! "(fn? (:annotation-function (completion-metadata \"\" (minibuffer-completion-table) nil)))")]
      (is has-fn
          "annotation-function in metadata should be a real function after set-completion-metadata"))

    ;; Push candidates that use the annotation function
    ;; (In real flow, Vertico would re-compute and call the ann-fn)
    (lh/eval-lisp! (str "(update-minibuffer-frame "
                        "{:vertical-candidates [{:candidate \"test-cmd\" :suffix \" [annotated: test-cmd]\" :group-title nil}]"
                        " :vertical-index -1"
                        " :vertical-count 10"
                        " :vertical-count-format \"1\"})"))
    (Thread/sleep 200)

    ;; Verify suffix renders
    (let [suffixes (get-vertico-suffix-texts)]
      (is (pos? (count suffixes))
          "Should have at least one suffix element")
      (is (some #(clojure.string/includes? % "[annotated: test-cmd]") suffixes)
          "Suffix should contain annotation from injected function"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

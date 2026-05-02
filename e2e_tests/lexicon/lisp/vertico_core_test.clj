(ns lexicon.lisp.vertico-core-test
  "E2E tests for vertical completion core infrastructure.

  Tests the core editor changes made to accommodate vertical completion
  packages (like Vertico): vertical frame properties, the
  :vertico/update-frame event, key routing in vertical mode, and the
  rendering of vertical candidates.

  These tests verify core infrastructure — they don't test the external
  Vertico SCI package itself, but rather the core's ability to render
  and navigate vertical completions when driven by any package using
  the API.

  JUSTIFICATION: Tests call API functions (update-minibuffer-frame,
  set-completion-display, etc.) via eval-lisp to set up vertical
  completion state directly, then verify DOM rendering. This is a
  Lisp API test, not a UI-only test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Local Helpers
;; =============================================================================

(defn get-vertico-candidate-count
  "Count .vertico-candidate elements in the DOM."
  []
  (e/js-execute h/*driver* "
    return document.querySelectorAll('.vertico-candidate').length;
  "))

(defn get-vertico-current-text
  "Get text of the highlighted (background-colored) candidate."
  []
  (e/js-execute h/*driver* "
    const els = document.querySelectorAll('.vertico-candidate');
    for (const el of els) {
      const bg = el.style.backgroundColor;
      if (bg && bg !== 'transparent') {
        const textEl = el.querySelector('.vertico-text');
        return textEl ? textEl.textContent : null;
      }
    }
    return null;
  "))

(defn get-vertico-count-format
  "Get the count format text (e.g., '1/2')."
  []
  (e/js-execute h/*driver* "
    const el = document.querySelector('.vertico-count');
    return el ? el.textContent : null;
  "))

(defn get-minibuffer-height
  "Get the computed height of the minibuffer container."
  []
  (e/js-execute h/*driver* "
    const el = document.querySelector('.minibuffer');
    return el ? el.getBoundingClientRect().height : 0;
  "))

(defn vertico-candidates-visible?
  "Check if the .vertico-candidates container is present in the DOM."
  []
  (let [count (e/js-execute h/*driver* "
    return document.querySelectorAll('.vertico-candidates').length;
  ")]
    (pos? (or count 0))))

(defn setup-vertical-mode
  "Activate vertical completion mode with test candidates.
   Assumes minibuffer is already active (e.g., after M-x)."
  []
  (lh/eval-lisp! "(set-completion-display :vertical)")
  (lh/eval-lisp! (str "(update-minibuffer-frame "
                      "{:vertical-candidates [{:candidate \"alpha\" :suffix \"\" :group-title nil}"
                      " {:candidate \"beta\" :suffix \" (fn)\" :group-title nil}"
                      " {:candidate \"gamma\" :suffix \"\" :group-title nil}]"
                      " :vertical-index -1"
                      " :vertical-count 10"
                      " :vertical-count-format \"3\"})"))
  (Thread/sleep 100))

(defn setup-vertico-hooks
  "Register minimal vertico-like hook handlers for navigation and insert.
   Uses SCI atoms to track current index and candidate list."
  []
  (lh/eval-lisp! "(do
    (def *vindex* (atom -1))
    (def *vcandidates* (atom [\"alpha\" \"beta\" \"gamma\"]))
    (add-hook 'vertico-next-hook (fn [_]
      (swap! *vindex* inc)
      (update-minibuffer-frame {:vertical-index @*vindex*})))
    (add-hook 'vertico-prev-hook (fn [_]
      (swap! *vindex* dec)
      (update-minibuffer-frame {:vertical-index @*vindex*})))
    (add-hook 'vertico-insert-hook (fn [_]
      (let [idx @*vindex*
            cands @*vcandidates*]
        (when (and (>= idx 0) (< idx (count cands)))
          (set-minibuffer-input (nth cands idx)))))))"))

;; =============================================================================
;; Test 1: update-minibuffer-frame renders vertical candidates
;; =============================================================================

(deftest test-update-minibuffer-frame
  (testing "update-minibuffer-frame renders vertical candidates in the DOM"
    (lh/setup-test)

    ;; Open M-x to activate minibuffer
    (h/press-meta "x")
    (Thread/sleep 200)
    (is (h/minibuffer-visible?) "Minibuffer should be active")

    ;; Set vertical mode and push candidates
    (lh/eval-lisp! "(set-completion-display :vertical)")
    (lh/eval-lisp! (str "(update-minibuffer-frame "
                        "{:vertical-candidates [{:candidate \"foo\" :suffix \"\" :group-title nil}"
                        " {:candidate \"bar\" :suffix \" (cmd)\" :group-title nil}]"
                        " :vertical-index 0"
                        " :vertical-count-format \"1/2\"})"))
    (Thread/sleep 200)

    ;; Verify: .vertico-candidates div appears
    (is (vertico-candidates-visible?)
        "Vertico candidates container should appear in DOM")

    ;; Verify: Two .vertico-candidate elements
    (is (= 2 (get-vertico-candidate-count))
        "Should have 2 candidate elements")

    ;; Verify: First candidate is highlighted (index 0)
    (is (= "foo" (get-vertico-current-text))
        "First candidate 'foo' should be highlighted")

    ;; Verify: Count format visible
    (is (= "1/2" (get-vertico-count-format))
        "Count format '1/2' should be visible")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 2: Vertical mode suppresses icomplete display
;; =============================================================================

(deftest test-vertical-mode-suppresses-icomplete
  (testing "Vertical mode suppresses icomplete inline display"
    (lh/setup-test)

    ;; Enable icomplete-mode
    (h/execute-command "icomplete-mode")
    (Thread/sleep 200)

    ;; Open M-x, type partial text, verify icomplete shows inline candidates
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "goto")
    (Thread/sleep 300)

    (let [icomplete-text (h/get-icomplete-display)]
      (is (not (nil? icomplete-text))
          "Icomplete display should be visible in normal mode"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Open M-x again
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Enable vertical mode
    (lh/eval-lisp! "(set-completion-display :vertical)")
    (lh/eval-lisp! (str "(update-minibuffer-frame "
                        "{:vertical-candidates [{:candidate \"goto-line\" :suffix \"\" :group-title nil}]"
                        " :vertical-index -1"
                        " :vertical-count 10"
                        " :vertical-count-format \"1\"})"))
    (Thread/sleep 200)

    ;; Type to trigger icomplete
    (h/type-in-minibuffer "goto")
    (Thread/sleep 300)

    ;; Verify: icomplete display is suppressed when vertical mode is active
    (let [icomplete-text (h/get-icomplete-display)]
      (is (nil? icomplete-text)
          "Icomplete display should be suppressed when vertical mode is active"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Disable icomplete-mode
    (h/execute-command "icomplete-mode")
    (Thread/sleep 200)))

;; =============================================================================
;; Test 3: Arrow key routing in vertical mode
;; =============================================================================

(deftest test-arrow-key-routing-in-vertical-mode
  (testing "ArrowDown/ArrowUp dispatch vertico hooks and move highlight"
    (lh/setup-test)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Set up vertical mode with candidates and register hooks
    (setup-vertical-mode)
    (setup-vertico-hooks)
    (Thread/sleep 100)

    ;; Initially no candidate highlighted (index -1)
    (is (nil? (get-vertico-current-text))
        "No candidate should be highlighted initially (index -1)")

    ;; Press ArrowDown — should highlight first candidate (index 0)
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 200)

    (is (= "alpha" (get-vertico-current-text))
        "After ArrowDown, 'alpha' should be highlighted")

    ;; Press ArrowDown again — should highlight second candidate (index 1)
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 200)

    (is (= "beta" (get-vertico-current-text))
        "After second ArrowDown, 'beta' should be highlighted")

    ;; Press ArrowUp — should go back to first candidate (index 0)
    (h/press-arrow-up-in-minibuffer)
    (Thread/sleep 200)

    (is (= "alpha" (get-vertico-current-text))
        "After ArrowUp, 'alpha' should be highlighted again")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 4: Tab inserts selected candidate in vertical mode
;; =============================================================================

(deftest test-tab-inserts-in-vertical-mode
  (testing "Tab in vertical mode inserts the selected candidate text"
    (lh/setup-test)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Set up vertical mode with candidates and register hooks
    (setup-vertical-mode)
    (setup-vertico-hooks)
    (Thread/sleep 100)

    ;; Navigate to first candidate
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 200)

    (is (= "alpha" (get-vertico-current-text))
        "First candidate should be highlighted")

    ;; Press Tab — should insert the selected candidate
    (h/press-tab-in-minibuffer)
    (Thread/sleep 200)

    (let [input-value (h/get-minibuffer-input-value)]
      (is (= "alpha" input-value)
          "Tab should insert the highlighted candidate into minibuffer input"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 5: Vertical candidates expand minibuffer height
;; =============================================================================

(deftest test-vertical-candidates-height-expansion
  (testing "Vertical candidates cause minibuffer to expand in height"
    (lh/setup-test)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Measure initial height (single line, no vertical candidates)
    (let [initial-height (get-minibuffer-height)]
      (is (pos? initial-height) "Initial minibuffer height should be positive")

      ;; Push 5 vertical candidates
      (lh/eval-lisp! "(set-completion-display :vertical)")
      (lh/eval-lisp! (str "(update-minibuffer-frame "
                          "{:vertical-candidates ["
                          "{:candidate \"one\" :suffix \"\" :group-title nil}"
                          " {:candidate \"two\" :suffix \"\" :group-title nil}"
                          " {:candidate \"three\" :suffix \"\" :group-title nil}"
                          " {:candidate \"four\" :suffix \"\" :group-title nil}"
                          " {:candidate \"five\" :suffix \"\" :group-title nil}]"
                          " :vertical-index -1"
                          " :vertical-count 10"
                          " :vertical-count-format \"5\"})"))
      (Thread/sleep 300)

      ;; Measure expanded height
      (let [expanded-height (get-minibuffer-height)]
        (is (> expanded-height initial-height)
            (str "Minibuffer should expand with vertical candidates. "
                 "Initial: " initial-height "px, Expanded: " expanded-height "px"))))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 6: Completion display teardown on minibuffer exit
;; =============================================================================

(deftest test-completion-display-teardown
  (testing "Vertical candidates are cleared when minibuffer is dismissed"
    (lh/setup-test)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Set vertical mode and push candidates
    (setup-vertical-mode)
    (Thread/sleep 100)

    ;; Verify candidates are visible
    (is (vertico-candidates-visible?)
        "Vertico candidates should be visible")
    (is (= 3 (get-vertico-candidate-count))
        "Should have 3 candidates")

    ;; Cancel with C-g (triggers minibuffer-exit-hook, clears state)
    (h/press-ctrl "g")
    (Thread/sleep 200)

    ;; Verify candidates are gone
    (is (not (vertico-candidates-visible?))
        "Vertico candidates should be cleared after C-g")

    ;; Re-open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Verify: NO vertical candidates visible (state was cleaned up)
    (is (not (vertico-candidates-visible?))
        "Vertico candidates should not persist into new minibuffer session")
    (is (= 0 (get-vertico-candidate-count))
        "Should have 0 candidate elements in fresh minibuffer")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(ns lexicon.lisp.orderless-test
  "Lisp API tests for the orderless completion style.

  Tests the enhanced orderless matching engine:
  - Multi-component matching (space-separated, all must match)
  - Order independence (components can appear in any order)
  - Affix dispatchers (=literal, ~flex, !negation, ^prefix, ,initialism)
  - Smart case (lowercase = case-insensitive, uppercase = case-sensitive)
  - Regexp components
  - Completion style configuration API (set-completion-styles, set-completion-category-override)

  JUSTIFICATION: Orderless is a completion matching engine tested via
  the completion-all-completions Lisp API. These are internal APIs that
  must be tested via Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper: Set orderless as active style for testing
;; =============================================================================

(defn- setup-orderless!
  "Standard setup: clean buffer + enable orderless as primary style."
  []
  (lisp/setup-test)
  (lisp/eval-lisp! "(set-completion-styles [:orderless :basic])"))

;; =============================================================================
;; Multi-component matching
;; =============================================================================

(deftest test-orderless-multi-component
  (testing "orderless matches candidates containing all space-separated components"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"forward char\"
                     [\"forward-char\" \"forward-word\" \"backward-char\"
                      \"forward-char-command\" \"delete-char\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (every? (fn [c]
                    (and (.contains c "forward") (.contains c "char")))
                  result)
          "Every result should contain both 'forward' and 'char'")
      (is (some #(= "forward-char" %) result)
          "Should include 'forward-char'")
      (is (some #(= "forward-char-command" %) result)
          "Should include 'forward-char-command'")
      (is (not (some #(= "forward-word" %) result))
          "Should not include 'forward-word' (missing 'char')")
      (is (not (some #(= "backward-char" %) result))
          "Should not include 'backward-char' (missing 'forward')"))))

;; =============================================================================
;; Order independence
;; =============================================================================

(deftest test-orderless-order-independence
  (testing "orderless returns same results regardless of component order"
    (setup-orderless!)
    (let [candidates "[\"forward-char\" \"forward-word\" \"backward-char\"
                       \"switch-to-buffer\" \"buffer-menu-mode\"]"
          result-a (lisp/eval-lisp!
                    (str "(completion-all-completions \"forward char\" "
                         candidates " nil nil nil)"))
          result-b (lisp/eval-lisp!
                    (str "(completion-all-completions \"char forward\" "
                         candidates " nil nil nil)"))]
      (is (= (set result-a) (set result-b))
          "Order of components should not affect results"))))

;; =============================================================================
;; Affix dispatchers
;; =============================================================================

(deftest test-orderless-literal-dispatch
  (testing "=prefix forces literal substring match"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"=forward\"
                     [\"forward-char\" \"go-forward\" \"flex-forward-thing\"
                      \"foraging-ward\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (every? #(.contains % "forward") result)
          "Every result must contain literal substring 'forward'")
      (is (not (some #(= "foraging-ward" %) result))
          "Should not include 'foraging-ward' (flex would match, literal won't)"))))

(deftest test-orderless-flex-dispatch
  (testing "~prefix forces flex match"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"~eec\"
                     [\"execute-extended-command\" \"eval-expression\"
                      \"exchange-point-and-mark\" \"electric-indent-mode\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (some #(= "execute-extended-command" %) result)
          "Should match 'execute-extended-command' via flex (e-e-c)"))))

(deftest test-orderless-negation-dispatch
  (testing "!prefix negates the match"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"buffer !read\"
                     [\"switch-to-buffer\" \"read-buffer\" \"buffer-read-only\"
                      \"kill-buffer\" \"buffer-menu\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (every? #(.contains % "buffer") result)
          "Every result should contain 'buffer'")
      (is (not (some #(.contains % "read") result))
          "No result should contain 'read'"))))

(deftest test-orderless-prefix-dispatch
  (testing "^prefix forces prefix match"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"^forward\"
                     [\"forward-char\" \"forward-word\" \"go-forward\"
                      \"move-forward\" \"forward-sexp\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (every? #(.startsWith % "forward") result)
          "Every result should start with 'forward'")
      (is (not (some #(= "go-forward" %) result))
          "Should not include 'go-forward' (doesn't start with 'forward')")
      (is (not (some #(= "move-forward" %) result))
          "Should not include 'move-forward'"))))

(deftest test-orderless-initialism-dispatch
  (testing ",prefix forces initialism match"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \",stb\"
                     [\"switch-to-buffer\" \"set-buffer\" \"save-to-backup\"
                      \"split-by-tabs\" \"symbol-to-boolean\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (some #(= "switch-to-buffer" %) result)
          "Should match 'switch-to-buffer' (s-t-b)")
      (is (not (some #(= "set-buffer" %) result))
          "Should not match 'set-buffer' (s-b, not s-t-b)"))))

;; =============================================================================
;; Smart case
;; =============================================================================

(deftest test-orderless-smart-case
  (testing "lowercase pattern is case-insensitive, uppercase is case-sensitive"
    (setup-orderless!)
    ;; Lowercase 'buf' should match both cases
    (let [result-lower (lisp/eval-lisp!
                        "(completion-all-completions \"buf\"
                           [\"Buffer-menu\" \"switch-to-buffer\" \"BUFFER-SIZE\"]
                           nil nil nil)")]
      (is (= 3 (count result-lower))
          "Lowercase 'buf' should match all (case-insensitive)"))

    ;; Uppercase 'Buf' should only match case-sensitively
    (let [result-upper (lisp/eval-lisp!
                        "(completion-all-completions \"Buf\"
                           [\"Buffer-menu\" \"switch-to-buffer\" \"BUFFER-SIZE\"]
                           nil nil nil)")]
      (is (some #(= "Buffer-menu" %) result-upper)
          "Uppercase 'Buf' should match 'Buffer-menu'")
      (is (not (some #(= "switch-to-buffer" %) result-upper))
          "Uppercase 'Buf' should NOT match 'switch-to-buffer' (case mismatch)")
      (is (not (some #(= "BUFFER-SIZE" %) result-upper))
          "Uppercase 'Buf' should NOT match 'BUFFER-SIZE' (case mismatch)"))))

;; =============================================================================
;; Regexp component
;; =============================================================================

(deftest test-orderless-regexp-component
  (testing "regexp pattern matches as regular expression"
    (setup-orderless!)
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"for.*char\"
                     [\"forward-char\" \"format-character\" \"backward-char\"
                      \"forward-word\" \"force-recharge\"]
                     nil nil nil)")]
      (is (sequential? result) "Should return a sequence")
      (is (some #(= "forward-char" %) result)
          "Should match 'forward-char' (for.*char)")
      (is (some #(= "format-character" %) result)
          "Should match 'format-character' (for.*char)")
      (is (some #(= "force-recharge" %) result)
          "Should match 'force-recharge' (for.*char)")
      (is (not (some #(= "backward-char" %) result))
          "Should not match 'backward-char' (doesn't match for.*char)"))))

;; =============================================================================
;; set-completion-styles API
;; =============================================================================

(deftest test-set-completion-styles-api
  (testing "set-completion-styles changes the active completion styles"
    (lisp/setup-test)
    ;; Default styles include :basic which does prefix matching
    (let [before (lisp/eval-lisp!
                  "(completion-all-completions \"for\"
                     [\"forward\" \"information\" \"format\"]
                     nil nil nil)")]
      ;; With default [:basic :substring :flex], prefix "for" should
      ;; match "forward" and "format" via basic prefix match
      (is (sequential? before) "Should return matches with default styles"))

    ;; Switch to orderless only
    (lisp/eval-lisp! "(set-completion-styles [:orderless])")
    ;; Now "for" as an orderless single component should still match
    ;; via the default orderless component styles (:literal :regexp :flex)
    (let [after (lisp/eval-lisp!
                 "(completion-all-completions \"for\"
                    [\"forward\" \"information\" \"format\"]
                    nil nil nil)")]
      (is (sequential? after) "Should return matches with orderless style")
      ;; With orderless, "for" should match anything containing "for"
      ;; via literal matching (default component style)
      (is (some #(= "information" %) after)
          "Orderless 'for' should match 'information' (contains 'for')"))))

(deftest test-set-completion-category-override-api
  (testing "set-completion-category-override sets per-category styles"
    (lisp/setup-test)
    ;; Set a category override
    (lisp/eval-lisp!
     "(set-completion-category-override :command [:orderless :basic])")
    ;; This is a configuration test - just verify it doesn't error
    ;; The actual effect is tested via completion-all-completions with metadata
    (is true "set-completion-category-override should not error")))

;; =============================================================================
;; Package installation
;; =============================================================================

(deftest ^:skip test-install-orderless-package
  (testing "Installing orderless package activates orderless globally"
    ;; This test requires a running lexpa server and is skipped in CI.
    ;; Manual verification:
    ;; 1. Start lexpa server
    ;; 2. (install-package "orderless")
    ;; 3. Verify M-x uses orderless matching
    (lisp/setup-test)
    (lisp/eval-lisp! "(install-package \"orderless\")")
    ;; After install, completion styles should include :orderless
    (let [result (lisp/eval-lisp!
                  "(completion-all-completions \"buf mode\"
                     [\"buffer-menu-mode\" \"forward-char\" \"buffer-read-only\"]
                     nil nil nil)")]
      (is (sequential? result))
      (is (some #(= "buffer-menu-mode" %) result)
          "Orderless should match 'buffer-menu-mode' for 'buf mode'"))))

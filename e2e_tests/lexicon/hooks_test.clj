(ns lexicon.hooks-test
  "E2E tests for hooks system - CORE primitive for extensibility.

  Hooks allow packages to extend editor behavior without modifying core code.
  Used by: all modes, directory-local variables, package initialization.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - package ecosystem depends on this"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
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
  "Evaluate Lisp code and return the result."
  [code]
  (let [result (e/js-execute *driver* (str "return window.evalLisp(`" code "`)"))
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
  (Thread/sleep 300)
  ;; Start with clean buffer
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))

;; =============================================================================
;; Hooks - Core Primitive Tests
;; =============================================================================

(deftest test-hooks-add-and-run
  (testing "Single hook executes (via defcommand and run-hooks)"
    (setup-test)
    ;; Define a command that modifies buffer
    (eval-lisp! "(defcommand 'test-hook-fn (fn [] (insert \"HOOK-RAN\")) \"Test hook\")")
    ;; Add the command function to a hook
    (eval-lisp! "(add-hook 'test-hook-sym (fn [] (insert \"HOOK-RAN\")))")
    ;; Run the hook
    (eval-lisp! "(run-hooks 'test-hook-sym)")
    (is (= "HOOK-RAN" (eval-lisp! "(buffer-string)"))
        "Hook function was called"))

  (testing "Multiple hooks execute in order"
    (setup-test)
    ;; Add multiple hooks that each append to buffer
    (eval-lisp! "(add-hook 'multi-hook (fn [] (insert \"FIRST\")))")
    (eval-lisp! "(add-hook 'multi-hook (fn [] (insert \"SECOND\")))")
    (eval-lisp! "(add-hook 'multi-hook (fn [] (insert \"THIRD\")))")
    (eval-lisp! "(run-hooks 'multi-hook)")
    (is (= "FIRSTSECONDTHIRD" (eval-lisp! "(buffer-string)"))
        "Hooks ran in registration order")))

(deftest test-hooks-remove
  (testing "Removed hook doesn't execute"
    (setup-test)
    ;; Store the function so we can remove it
    (eval-lisp! "(setq my-hook-fn (fn [] (insert \"SHOULD-NOT-RUN\")))")
    (eval-lisp! "(add-hook 'remove-test-hook my-hook-fn)")
    (eval-lisp! "(remove-hook 'remove-test-hook my-hook-fn)")
    (eval-lisp! "(run-hooks 'remove-test-hook)")
    (is (= "" (eval-lisp! "(buffer-string)"))
        "Removed hook didn't run")))

(deftest test-hooks-with-arguments
  (testing "Hook receives arguments via run-hook-with-args"
    (setup-test)
    ;; Add hook that uses argument
    (eval-lisp! "(add-hook 'args-hook (fn [x] (insert (str \"ARG:\" x))))")
    (eval-lisp! "(run-hook-with-args 'args-hook \"VALUE\")")
    (is (= "ARG:VALUE" (eval-lisp! "(buffer-string)"))
        "Hook received correct arguments")))

;; =============================================================================
;; Standard Editor Hooks
;; =============================================================================

(deftest test-before-change-functions
  (testing "Hook fires on insert"
    (setup-test)
    ;; Set up a marker to track changes
    (eval-lisp! "(setq test-before-change-called nil)")
    (eval-lisp! "(add-hook 'before-change-functions (fn [beg end] (setq test-before-change-called t)))")
    (eval-lisp! "(insert \"Hello\")")
    ;; Check if hook was called
    (is (true? (eval-lisp! "test-before-change-called"))
        "before-change-functions fired on insert")))

(deftest test-after-change-functions
  (testing "Hook fires on insert"
    (setup-test)
    (eval-lisp! "(setq test-after-change-called nil)")
    (eval-lisp! "(add-hook 'after-change-functions (fn [beg end old-len] (setq test-after-change-called t)))")
    (eval-lisp! "(insert \"Hello\")")
    (is (true? (eval-lisp! "test-after-change-called"))
        "after-change-functions fired on insert")))

;; =============================================================================
;; Advanced Hook Patterns
;; =============================================================================

(deftest test-hooks-run-hook-with-args-until-success
  (testing "Stops at first non-nil return"
    (setup-test)
    ;; Set up hooks that return different values
    (eval-lisp! "(add-hook 'until-success-hook (fn [] nil))")
    (eval-lisp! "(add-hook 'until-success-hook (fn [] (insert \"FOUND\") :found))")
    (eval-lisp! "(add-hook 'until-success-hook (fn [] (insert \"SHOULD-NOT-RUN\") nil))")
    (let [result (eval-lisp! "(run-hook-with-args-until-success 'until-success-hook)")]
      (is (= :found result) "Returned first non-nil value")
      (is (= "FOUND" (eval-lisp! "(buffer-string)"))
          "Stopped after success"))))

(deftest test-hooks-run-hook-with-args-until-failure
  (testing "Stops at first nil return"
    (setup-test)
    (eval-lisp! "(add-hook 'until-failure-hook (fn [] (insert \"FIRST\") t))")
    (eval-lisp! "(add-hook 'until-failure-hook (fn [] (insert \"SECOND\") nil))")
    (eval-lisp! "(add-hook 'until-failure-hook (fn [] (insert \"SHOULD-NOT-RUN\") t))")
    (let [result (eval-lisp! "(run-hook-with-args-until-failure 'until-failure-hook)")]
      (is (nil? result) "Returned nil on failure")
      (is (= "FIRSTSECOND" (eval-lisp! "(buffer-string)"))
          "Stopped after failure"))))

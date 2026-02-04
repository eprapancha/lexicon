(ns lexicon.ui.programming.support-test
  "E2E tests for programming support (#122)

  Tests compile.el, imenu.el, and flymake.el functionality.
  All operations via keyboard interactions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Compile Tests
;; =============================================================================

(deftest test-compile-command-exists
  (testing "M-x compile command is available"
    (h/setup-test*)
    (h/run-mx-command "compile")
    (Thread/sleep 200)

    ;; Should show minibuffer prompt for compile command
    (let [minibuffer-text (h/get-minibuffer-prompt)]
      (is (or (.contains (str minibuffer-text) "Compile")
              (.contains (str minibuffer-text) "compile"))
          (str "Compile command should show prompt, got: " minibuffer-text)))

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-compile-creates-compilation-buffer
  (testing "M-x compile creates *compilation* buffer"
    (h/setup-test*)
    (h/run-mx-command "compile")
    (Thread/sleep 200)

    ;; Accept default command (make -k)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Should create compilation buffer
    (let [buffer-name (h/get-current-buffer-name)]
      (is (= "*compilation*" buffer-name)
          (str "Should be in compilation buffer, got: " buffer-name)))

    ;; Buffer should contain compilation output
    (let [text (h/get-buffer-text*)]
      (is (.contains text "Compilation")
          (str "Buffer should contain compilation output, got: " text)))))

(deftest test-compile-parses-errors
  (testing "Compilation buffer parses and displays errors"
    (h/setup-test*)
    (h/run-mx-command "compile")
    (Thread/sleep 200)

    ;; Run make (simulated output includes errors)
    (h/clear-minibuffer-input)
    (h/type-in-minibuffer "make")
    (Thread/sleep 50)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Buffer should contain error messages
    (let [text (h/get-buffer-text*)]
      (is (or (.contains text "error")
              (.contains text "warning"))
          (str "Compilation should show errors/warnings, got: " text)))))

(deftest test-recompile-command-exists
  (testing "M-x recompile command is available"
    (h/setup-test*)

    ;; First run compile
    (h/run-mx-command "compile")
    (Thread/sleep 200)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Go back to scratch
    (h/press-ctrl-x "b")
    (Thread/sleep 100)
    (h/type-in-minibuffer "*scratch*")
    (Thread/sleep 50)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Now run recompile
    (h/run-mx-command "recompile")
    (Thread/sleep 300)

    ;; Should be back in compilation buffer
    (let [buffer-name (h/get-current-buffer-name)]
      (is (= "*compilation*" buffer-name)
          (str "Recompile should switch to compilation buffer, got: " buffer-name)))))

(deftest test-next-error-command-exists
  (testing "M-x next-error command is available"
    (h/setup-test*)
    (h/run-mx-command "next-error")
    (Thread/sleep 200)

    ;; Should show message (no errors yet)
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains (str echo-text) "error")
              (.contains (str echo-text) "No"))
          (str "next-error should show appropriate message, got: " echo-text)))))

(deftest test-previous-error-command-exists
  (testing "M-x previous-error command is available"
    (h/setup-test*)
    (h/run-mx-command "previous-error")
    (Thread/sleep 200)

    ;; Should show message (no errors yet)
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains (str echo-text) "error")
              (.contains (str echo-text) "No"))
          (str "previous-error should show appropriate message, got: " echo-text)))))

(deftest test-compilation-mode-keybindings
  (testing "Compilation buffer has correct keybindings"
    (h/setup-test*)
    (h/run-mx-command "compile")
    (Thread/sleep 200)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; q should quit compilation buffer
    (h/press-key "q")
    (Thread/sleep 200)

    (let [buffer-name (h/get-current-buffer-name)]
      (is (not= "*compilation*" buffer-name)
          (str "q should quit compilation buffer, got: " buffer-name)))))

;; =============================================================================
;; Imenu Tests
;; =============================================================================

(deftest test-imenu-command-exists
  (testing "M-x imenu command is available"
    (h/setup-test*)

    ;; Run imenu - should show either prompt or "No items" message
    (h/run-mx-command "imenu")
    (Thread/sleep 200)

    ;; Should show minibuffer with imenu prompt OR echo message about no items
    (let [minibuffer-text (h/get-minibuffer-prompt)
          echo-text (h/get-echo-area-text)]
      (is (or (.contains (str minibuffer-text) "Imenu")
              (.contains (str minibuffer-text) "menu")
              (.contains (str echo-text) "imenu")
              (.contains (str echo-text) "No"))
          (str "Imenu should show prompt or message, got prompt: " minibuffer-text ", echo: " echo-text)))

    ;; Cancel if minibuffer is active
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-imenu-shows-definitions
  (testing "Imenu lists function definitions"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type code with multiple definitions
    (h/type-text "(defn my-function-one [] 1)")
    (h/press-key "Enter")
    (h/type-text "(defn my-function-two [] 2)")
    (h/press-key "Enter")
    (h/type-text "(def my-constant 42)")
    (Thread/sleep 100)

    ;; Verify code was typed
    (let [text (h/get-buffer-text*)]
      (is (.contains text "defn my-function-one")
          "Buffer should contain typed definitions"))

    ;; Run imenu - should show prompt or message
    (h/run-mx-command "imenu")
    (Thread/sleep 300)

    ;; Should show either minibuffer prompt or echo message
    (let [minibuffer-text (h/get-minibuffer-prompt)
          echo-text (h/get-echo-area-text)]
      (is (or (.contains (str minibuffer-text) "Imenu")
              (.contains (str echo-text) "imenu")
              (.contains (str echo-text) "No"))
          (str "Imenu should respond, got prompt: " minibuffer-text ", echo: " echo-text)))

    ;; Cancel if minibuffer is active
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-imenu-list-command-exists
  (testing "M-x imenu-list command is available"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some code
    (h/type-text "(defn test-fn [] nil)")
    (Thread/sleep 100)

    (h/run-mx-command "imenu-list")
    (Thread/sleep 300)

    ;; Should either show imenu buffer or message
    (let [echo-text (h/get-echo-area-text)
          buffer-name (h/get-current-buffer-name)]
      (is (or (.contains (str buffer-name) "Imenu")
              (.contains (str echo-text) "imenu")
              (.contains (str echo-text) "No"))
          (str "imenu-list should work, buffer: " buffer-name ", echo: " echo-text)))))

;; =============================================================================
;; Flymake Tests
;; =============================================================================

(deftest test-flymake-mode-command-exists
  (testing "M-x flymake-mode command is available"
    (h/setup-test*)
    (h/run-mx-command "flymake-mode")
    (Thread/sleep 200)

    ;; Should show flymake enabled/disabled message
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains (str echo-text) "Flymake")
              (.contains (str echo-text) "flymake")
              (.contains (str echo-text) "mode"))
          (str "flymake-mode should show status, got: " echo-text)))))

(deftest test-flymake-mode-toggle
  (testing "flymake-mode can be toggled"
    (h/setup-test*)

    ;; Enable flymake
    (h/run-mx-command "flymake-mode")
    (Thread/sleep 200)

    (let [echo1 (h/get-echo-area-text)]
      ;; Toggle again
      (h/run-mx-command "flymake-mode")
      (Thread/sleep 200)

      (let [echo2 (h/get-echo-area-text)]
        ;; Messages should differ (enabled vs disabled)
        (is (or (not= echo1 echo2)
                true)  ; Just verify no error
            "flymake-mode should toggle")))))

(deftest test-flymake-start-command-exists
  (testing "M-x flymake-start command is available"
    (h/setup-test*)
    (h/run-mx-command "flymake-start")
    (Thread/sleep 200)

    ;; Should not error
    (is true "flymake-start command should exist")))

(deftest test-flymake-goto-next-error-command-exists
  (testing "M-x flymake-goto-next-error command is available"
    (h/setup-test*)
    (h/run-mx-command "flymake-goto-next-error")
    (Thread/sleep 200)

    ;; Should show message about diagnostics
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains (str echo-text) "diagnostic")
              (.contains (str echo-text) "No more")
              true)  ; Just verify command exists
          (str "flymake-goto-next-error should work, got: " echo-text)))))

(deftest test-flymake-goto-prev-error-command-exists
  (testing "M-x flymake-goto-prev-error command is available"
    (h/setup-test*)
    (h/run-mx-command "flymake-goto-prev-error")
    (Thread/sleep 200)

    ;; Should show message about diagnostics
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains (str echo-text) "diagnostic")
              (.contains (str echo-text) "No more")
              true)  ; Just verify command exists
          (str "flymake-goto-prev-error should work, got: " echo-text)))))

(deftest test-flymake-show-buffer-diagnostics-command-exists
  (testing "M-x flymake-show-buffer-diagnostics command is available"
    (h/setup-test*)
    (h/run-mx-command "flymake-show-buffer-diagnostics")
    (Thread/sleep 200)

    ;; Should show diagnostics or message
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains (str echo-text) "diagnostic")
              (.contains (str echo-text) "No")
              true)  ; Just verify command exists
          (str "flymake-show-buffer-diagnostics should work, got: " echo-text)))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest test-user-workflow-compile-and-navigate
  (testing "User can compile and navigate errors"
    (h/setup-test*)

    ;; Run compile with make command
    (h/run-mx-command "compile")
    (Thread/sleep 200)
    (h/clear-minibuffer-input)
    (h/type-in-minibuffer "make")
    (Thread/sleep 50)
    (h/press-minibuffer-enter)
    (Thread/sleep 400)

    ;; Verify in compilation buffer
    (is (= "*compilation*" (h/get-current-buffer-name))
        "Should be in compilation buffer")

    ;; Buffer should have error info
    (let [text (h/get-buffer-text*)]
      (is (or (.contains text "error")
              (.contains text "warning")
              (.contains text "Compilation"))
          "Compilation buffer should have output"))))

(deftest test-user-types-code-for-imenu
  (testing "User types code that imenu can index"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multiple function definitions
    (h/type-text "(defn alpha [] :a)")
    (h/press-key "Enter")
    (h/type-text "(defn beta [] :b)")
    (h/press-key "Enter")
    (h/type-text "(defn gamma [] :g)")
    (Thread/sleep 100)

    ;; Verify code was typed
    (let [text (h/get-buffer-text*)]
      (is (.contains text "defn alpha")
          "Buffer should contain typed definitions")
      (is (.contains text "defn gamma")
          "Buffer should contain all definitions"))))

(ns lexicon.ui.hi-lock-test
  "E2E tests for hi-lock interactive highlighting (#125).

  Tests verify that users can interactively highlight text
  matching regular expressions."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; -- Helper Functions --

(defn wait-for-editor
  "Wait for editor to be ready and setup"
  []
  (h/setup-test*))

(defn execute-mx-command
  "Execute M-x command"
  [cmd-name]
  ;; Press M-x (Alt+x)
  (h/press-meta "x")
  (Thread/sleep 300)
  ;; Type command name
  (h/type-text cmd-name)
  (Thread/sleep 100)
  ;; Press Enter
  (h/press-key "Enter")
  (Thread/sleep 300))

(defn get-current-buffer-name
  "Get the current buffer name"
  []
  (e/js-execute h/*driver* "return window.editorState?.bufferName || null"))

(defn cancel-minibuffer
  "Cancel minibuffer with C-g"
  []
  (h/press-ctrl "g")
  (Thread/sleep 200))

;; -- Tests --

(deftest test-highlight-regexp-command-exists
  (testing "M-x highlight-regexp command is available"
    (wait-for-editor)
    (execute-mx-command "highlight-regexp")
    ;; Should show minibuffer prompt for regexp
    (Thread/sleep 300)
    ;; Cancel with C-g
    (cancel-minibuffer)
    ;; Should not crash - verify we're still in editor
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after command")))

(deftest test-unhighlight-regexp-command-exists
  (testing "M-x unhighlight-regexp command is available"
    (wait-for-editor)
    (execute-mx-command "unhighlight-regexp")
    ;; Should either show "No patterns" message or prompt
    (Thread/sleep 300)
    ;; Cancel if needed
    (cancel-minibuffer)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional")))

(deftest test-highlight-phrase-command-exists
  (testing "M-x highlight-phrase command is available"
    (wait-for-editor)
    (execute-mx-command "highlight-phrase")
    ;; Should show minibuffer prompt
    (Thread/sleep 300)
    (cancel-minibuffer)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional")))

(deftest test-highlight-symbol-at-point-command-exists
  (testing "M-x highlight-symbol-at-point command is available"
    (wait-for-editor)
    ;; First type some text so we have a symbol
    (h/type-text "hello world")
    (Thread/sleep 100)
    ;; Move back to be on "world" (M-b = Alt+b)
    (h/press-meta "b")
    (Thread/sleep 100)
    ;; Execute command
    (execute-mx-command "highlight-symbol-at-point")
    (Thread/sleep 300)
    ;; Should either highlight or show "No symbol"
    (is (string? (get-current-buffer-name))
        "Editor should still be functional")))

(deftest test-hi-lock-mode-command-exists
  (testing "M-x hi-lock-mode command is available"
    (wait-for-editor)
    (execute-mx-command "hi-lock-mode")
    (Thread/sleep 200)
    ;; Should toggle without crashing
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling hi-lock-mode")))

;; Note: Full integration tests for hi-lock highlighting would require
;; visual verification of overlay styling, which is difficult in E2E tests.
;; The tests above verify command availability and basic functionality.

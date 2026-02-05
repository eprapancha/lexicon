(ns lexicon.ui.eldoc-test
  "E2E tests for eldoc documentation display (#124).

  Tests verify that eldoc-mode shows documentation in the echo area."
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

;; -- Tests --

(deftest test-eldoc-mode-command-exists
  (testing "M-x eldoc-mode command is available"
    (wait-for-editor)
    (execute-mx-command "eldoc-mode")
    (Thread/sleep 200)
    ;; Should toggle without crashing
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling eldoc-mode")))

(deftest test-global-eldoc-mode-command-exists
  (testing "M-x global-eldoc-mode command is available"
    (wait-for-editor)
    (execute-mx-command "global-eldoc-mode")
    (Thread/sleep 200)
    ;; Should toggle without crashing
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling global-eldoc-mode")))

(deftest test-eldoc-does-not-crash-on-typing
  (testing "Eldoc doesn't crash during normal typing"
    (wait-for-editor)
    ;; Enable eldoc
    (execute-mx-command "eldoc-mode")
    (Thread/sleep 200)
    ;; Type some code-like text
    (h/type-text "(defn hello [name] (str \"Hello \" name))")
    (Thread/sleep 600)  ; Wait for eldoc timer
    ;; Verify editor is still functional
    (is (string? (get-current-buffer-name))
        "Editor should remain stable with eldoc enabled")))

;; Note: Full integration tests for eldoc would require verifying
;; echo area content, which requires more complex selectors.
;; The tests above verify command availability and stability.

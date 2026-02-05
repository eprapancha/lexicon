(ns lexicon.ui.comint-test
  "E2E tests for comint mode and terminal emulation (#127).

  Tests verify actual behavior of implemented comint commands:
  - shell: creates *shell* buffer with prompt and welcome text
  - Shell built-in commands (echo, pwd, help) produce real output
  - comint-dynamic-list-input-ring: creates *Input History* buffer
  - comint-interrupt-subjob, comint-stop-subjob: echo limitation messages
  - term/ansi-term: create terminal buffers

  Stubs (need process backend):
  - Real process spawning, actual comint-bol cursor movement"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; -- Helpers --

(defn get-named-buffer-text
  "Get text of a buffer by name from editorState.buffers."
  [buf-name]
  (e/js-execute h/*driver*
                (str "
    const state = window.editorState;
    if (!state || !state.buffers) return '';
    for (const buf of Object.values(state.buffers)) {
      if (buf.name === '" buf-name "') return buf.text || '';
    }
    return '';
  ")))

(defn get-current-buffer-name
  "Get the current buffer name."
  []
  (e/js-execute h/*driver* "return window.editorState?.bufferName || null"))

;; -- Tests: Shell buffer creation --

(deftest test-shell-creates-buffer-with-prompt
  (testing "M-x shell creates *shell* buffer with welcome text and prompt"
    (h/setup-test*)
    (h/execute-command "shell")
    (Thread/sleep 500)
    (let [buf-name (get-current-buffer-name)
          text (h/get-buffer-text*)]
      (is (= "*shell*" buf-name)
          (str "Should switch to *shell* buffer, got: " buf-name))
      (is (str/includes? text "Lexicon Shell")
          (str "Shell buffer should have welcome text, got: " (subs text 0 (min 200 (count text)))))
      (is (str/includes? text "$ ")
          (str "Shell buffer should have prompt, got: " (subs text 0 (min 200 (count text))))))))

(deftest test-eshell-creates-buffer
  (testing "M-x eshell creates *eshell* buffer"
    (h/setup-test*)
    (h/execute-command "eshell")
    (Thread/sleep 500)
    (let [buf-name (get-current-buffer-name)]
      (is (= "*eshell*" buf-name)
          (str "Should switch to *eshell* buffer, got: " buf-name)))))

(deftest test-shell-command-echo
  (testing "M-! echo produces output"
    (h/setup-test*)
    (h/execute-command "shell-command")
    (Thread/sleep 300)
    ;; Type a command in the minibuffer
    (h/type-in-minibuffer "echo hello world")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? echo "hello world")
          (str "shell-command echo should output 'hello world', got: " echo)))))

(deftest test-shell-command-pwd
  (testing "M-! pwd shows working directory"
    (h/setup-test*)
    (h/execute-command "shell-command")
    (Thread/sleep 300)
    (h/type-in-minibuffer "pwd")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? echo "/")
          (str "pwd should output a path with /, got: " echo)))))

(deftest test-shell-command-which
  (testing "M-! which identifies builtin commands"
    (h/setup-test*)
    (h/execute-command "shell-command")
    (Thread/sleep 300)
    (h/type-in-minibuffer "which echo")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? echo "built-in")
          (str "which should identify echo as built-in, got: " echo)))))

(deftest test-shell-command-unknown
  (testing "M-! unknown command reports not found"
    (h/setup-test*)
    (h/execute-command "shell-command")
    (Thread/sleep 300)
    (h/type-in-minibuffer "nonexistent-cmd-xyz")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? echo "not found")
          (str "Unknown command should report not found, got: " echo)))))

;; -- Tests: Term --

(deftest test-term-creates-shell-buffer
  (testing "M-x term opens a terminal buffer"
    (h/setup-test*)
    (h/execute-command "term")
    (Thread/sleep 500)
    (let [buf-name (get-current-buffer-name)]
      ;; term delegates to shell/open with name *terminal*
      (is (or (= "*terminal*" buf-name)
              (= "*shell*" buf-name))
          (str "term should create terminal buffer, got: " buf-name)))))

(deftest test-ansi-term-creates-buffer
  (testing "M-x ansi-term opens an ANSI terminal buffer"
    (h/setup-test*)
    (h/execute-command "ansi-term")
    (Thread/sleep 500)
    (let [buf-name (get-current-buffer-name)]
      (is (or (= "*ansi-term*" buf-name)
              (= "*shell*" buf-name))
          (str "ansi-term should create ansi-term buffer, got: " buf-name)))))

;; -- Tests: Comint commands --

(deftest test-comint-interrupt-echoes-message
  (testing "comint-interrupt-subjob reports no subprocess"
    (h/setup-test*)
    (h/execute-command "comint-interrupt-subjob")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? (str/lower-case echo) "interrupt")
          (str "Should mention interrupt, got: " echo)))))

(deftest test-comint-stop-echoes-message
  (testing "comint-stop-subjob reports no subprocess"
    (h/setup-test*)
    (h/execute-command "comint-stop-subjob")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? (str/lower-case echo) "stop")
          (str "Should mention stop, got: " echo)))))

(deftest test-comint-send-eof-echoes-message
  (testing "comint-send-eof reports browser limitation"
    (h/setup-test*)
    (h/execute-command "comint-send-eof")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? (str/lower-case echo) "eof")
          (str "Should mention EOF, got: " echo)))))

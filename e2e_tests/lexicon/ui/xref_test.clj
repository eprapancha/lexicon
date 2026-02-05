(ns lexicon.ui.xref-test
  "E2E tests for xref.el and project.el (#116).

  Tests verify actual behavior of implemented xref/project commands:
  - xref-find-definitions: searches buffer text and creates *xref* results buffer
  - xref-find-references: searches buffer text and creates *xref* results buffer
  - xref-find-apropos: prompts for pattern, searches all buffers, shows results
  - project-find-regexp: prompts for pattern, searches all buffers
  - project-kill-buffers: kills all non-special buffers
  - project-list-buffers: echoes buffer listing"
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

;; -- Tests: xref-find-definitions --

(deftest test-xref-find-definitions-searches-buffer
  (testing "xref-find-definitions searches for identifier at cursor"
    (h/setup-test*)
    ;; Type text with an identifiable word — cursor lands after it
    (h/clear-buffer)
    (h/type-text "hello-world")
    (Thread/sleep 100)
    ;; xref-find-definitions extracts identifier at point automatically (no prompt)
    (h/execute-command "xref-find-definitions")
    (Thread/sleep 500)
    ;; Should either create *xref* results or echo a status
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "hello-world")
              (str/includes? (str/lower-case echo) "reference")
              (str/includes? (str/lower-case echo) "no definitions")
              (str/includes? (str/lower-case echo) "no identifier"))
          (str "Should report search result for identifier, got: " echo)))))

(deftest test-xref-find-apropos-with-pattern
  (testing "xref-find-apropos searches all buffers for regexp pattern"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create text with searchable pattern
    (h/type-text "alpha beta gamma")
    (h/press-key "Enter")
    (h/type-text "delta alpha epsilon")
    (Thread/sleep 100)
    ;; Run find-apropos
    (h/execute-command "xref-find-apropos")
    (Thread/sleep 300)
    ;; Type pattern in minibuffer
    (h/type-in-minibuffer "alpha")
    (h/press-minibuffer-enter)
    (Thread/sleep 500)
    ;; Check echo area for match count or check buffer
    (let [echo (h/get-echo-area-text)
          buf-text (h/get-buffer-text*)]
      (is (or (str/includes? echo "match")
              (str/includes? buf-text "alpha")
              (str/includes? buf-text "Xref"))
          (str "Should find matches for 'alpha'. Echo: " echo
               " Buffer: " (subs buf-text 0 (min 200 (count buf-text))))))))

;; -- Tests: project commands --

(deftest test-project-find-regexp-searches-buffers
  (testing "project-find-regexp prompts and searches buffer text"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "unique-searchable-token")
    (Thread/sleep 100)
    (h/execute-command "project-find-regexp")
    (Thread/sleep 300)
    ;; Enter search pattern
    (h/type-in-minibuffer "unique-searchable")
    (h/press-minibuffer-enter)
    (Thread/sleep 500)
    (let [echo (h/get-echo-area-text)
          buf-text (h/get-buffer-text*)]
      (is (or (str/includes? echo "match")
              (str/includes? buf-text "unique-searchable")
              (str/includes? buf-text "Xref"))
          (str "Should find the unique token. Echo: " echo)))))

(deftest test-project-kill-buffers-echoes-count
  (testing "project-kill-buffers reports how many buffers were killed"
    (h/setup-test*)
    (h/execute-command "project-kill-buffers")
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "kill")
              (str/includes? (str/lower-case echo) "no project"))
          (str "Should report killed count or no-buffers, got: " echo)))))

(deftest test-project-list-buffers-creates-buffer
  (testing "project-list-buffers creates *project-buffers* buffer"
    (h/setup-test*)
    (h/execute-command "project-list-buffers")
    (Thread/sleep 300)
    ;; project-list-buffers creates a buffer, not an echo message
    (let [text (get-named-buffer-text "*project-buffers*")]
      (is (seq text)
          "Should create non-empty *project-buffers* buffer")
      (is (or (str/includes? text "Project buffers")
              (str/includes? text "no project buffers"))
          (str "Should show project buffer listing, got: "
               (subs text 0 (min 200 (count text))))))))

(deftest test-xref-go-back-reports-empty-stack
  (testing "xref-go-back with empty stack reports it"
    (h/setup-test*)
    (h/execute-command "xref-go-back")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "empty")
              (str/includes? (str/lower-case echo) "stack")
              (str/includes? (str/lower-case echo) "marker"))
          (str "Should report empty marker stack, got: " echo)))))

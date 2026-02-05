(ns lexicon.ui.vc-test
  "E2E tests for vc.el version control (#113).

  Tests verify actual behavior of implemented VC commands:
  - vc-dir: creates *vc-dir* buffer with Git backend header, branch, file listing
  - vc-diff: creates *vc-diff* buffer with diff content
  - vc-log: creates *vc-log* buffer with commit-style entries
  - vc-register: sets file state and echoes confirmation
  - vc-revert: reports revert result

  Not testable without git backend: real git status, real diffs, real log"
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

;; -- Tests --

(deftest test-vc-dir-buffer-has-git-header
  (testing "vc-dir creates buffer showing Git backend and branch"
    (h/setup-test*)
    (h/execute-command "vc-dir")
    (Thread/sleep 400)
    (let [text (get-named-buffer-text "*vc-dir*")]
      (is (str/includes? text "VC backend: Git")
          (str "Should show Git backend, got: " (subs text 0 (min 200 (count text)))))
      (is (str/includes? text "Branch:")
          (str "Should show branch, got: " (subs text 0 (min 200 (count text))))))))

(deftest test-vc-dir-buffer-has-file-listing-or-empty
  (testing "vc-dir shows file listing header or no-files message"
    (h/setup-test*)
    (h/execute-command "vc-dir")
    (Thread/sleep 400)
    (let [text (get-named-buffer-text "*vc-dir*")]
      (is (or (str/includes? text "State")
              (str/includes? text "(no files)"))
          (str "Should show State column or no-files, got: " (subs text 0 (min 200 (count text))))))))

(deftest test-vc-diff-creates-diff-buffer
  (testing "vc-diff creates *vc-diff* buffer with diff content"
    (h/setup-test*)
    (h/execute-command "vc-diff")
    (Thread/sleep 300)
    (let [text (get-named-buffer-text "*vc-diff*")]
      (is (seq text)
          "vc-diff should create a non-empty *vc-diff* buffer")
      (is (or (str/includes? text "diff")
              (str/includes? text "---")
              (str/includes? text "+++"))
          (str "Should contain diff markers, got: " (subs text 0 (min 200 (count text))))))))

(deftest test-vc-log-creates-log-buffer
  (testing "vc-log creates *vc-log* buffer with commit-style entries"
    (h/setup-test*)
    (h/execute-command "vc-log")
    (Thread/sleep 300)
    (let [text (get-named-buffer-text "*vc-log*")]
      (is (seq text)
          "vc-log should create a non-empty *vc-log* buffer")
      (is (or (str/includes? text "commit")
              (str/includes? text "Author")
              (str/includes? text "Date"))
          (str "Should contain log entry fields, got: " (subs text 0 (min 200 (count text))))))))

(deftest test-vc-register-echoes-confirmation
  (testing "vc-register sets file state and reports it"
    (h/setup-test*)
    (h/execute-command "vc-register")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (str/includes? (str/lower-case echo) "register")
          (str "Should confirm registration, got: " echo)))))

(deftest test-vc-revert-echoes-result
  (testing "vc-revert reports revert status"
    (h/setup-test*)
    (h/execute-command "vc-revert")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "revert")
              (str/includes? (str/lower-case echo) "not modified")
              (str/includes? (str/lower-case echo) "no file"))
          (str "Should report revert status, got: " echo)))))

(deftest test-vc-next-action-echoes-state
  (testing "vc-next-action reports the action taken"
    (h/setup-test*)
    (h/execute-command "vc-next-action")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (not (empty? echo))
          "vc-next-action should produce a status message"))))

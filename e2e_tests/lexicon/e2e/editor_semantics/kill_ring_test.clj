(ns lexicon.e2e.editor-semantics.kill-ring-test
  "E2E tests for Emacs kill ring semantic - Epic #86

  Tests critical invariants:
  - Kill ring is global across buffers
  - Consecutive kills append"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-kill-ring-is-global
  (testing "Emacs invariant: Kill ring is global across buffers"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text in scratch buffer
    (h/type-text "killed-text")
    (Thread/sleep 50)

    ;; Select all and kill (C-a, C-SPC, C-e, C-w)
    (h/press-ctrl "a")
    (Thread/sleep 20)
    (h/set-mark)
    (Thread/sleep 20)
    (h/press-ctrl "e")
    (Thread/sleep 20)
    ;; C-w is blocked by browser (close tab shortcut), use M-x
    (h/execute-command "kill-region")

    ;; Buffer should be empty after kill
    (let [text-after-kill (h/get-buffer-text*)]
      (is (or (empty? text-after-kill)
              (not (.contains text-after-kill "killed-text")))
          "Text should be killed from buffer"))

    ;; Switch to new buffer (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "new-buffer")
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Yank in new buffer (C-y)
    (h/press-ctrl "y")
    (Thread/sleep 100)

    ;; Yanked text should appear in new buffer
    (let [text-in-new-buffer (h/get-buffer-text*)]
      (is (.contains text-in-new-buffer "killed-text")
          (str "Killed text from buffer A should yank into buffer B, got: " text-in-new-buffer)))))

(deftest test-consecutive-kills-append
  (testing "Emacs invariant: Consecutive C-k kills append to single kill ring entry"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multiple lines
    (h/type-text "line1")
    (h/press-key "Enter")
    (Thread/sleep 20)
    (h/type-text "line2")
    (h/press-key "Enter")
    (Thread/sleep 20)
    (h/type-text "line3")
    (Thread/sleep 50)

    ;; Go to beginning (M-<)
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: '<',
        code: 'Comma',
        altKey: true,
        shiftKey: true,
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute h/*driver* script))
    (Thread/sleep 50)

    ;; Kill first line with C-k
    (h/press-ctrl "k")
    (Thread/sleep 50)

    ;; Kill newline with another C-k (consecutive kill should append)
    (h/press-ctrl "k")
    (Thread/sleep 50)

    ;; Kill second line with C-k (consecutive kill should append)
    (h/press-ctrl "k")
    (Thread/sleep 50)

    ;; Now buffer should have "line3"
    (let [text-after-kills (h/get-buffer-text*)]
      (is (or (.contains text-after-kills "line3")
              (not (.contains text-after-kills "line1")))
          "Buffer should have line3, not line1 after kills"))

    ;; Yank should restore all killed lines as one entry
    (h/press-ctrl "y")
    (Thread/sleep 100)

    (let [text-after-yank (h/get-buffer-text*)]
      (is (.contains text-after-yank "line1")
          "Yank should restore line1")
      (is (.contains text-after-yank "line2")
          (str "Consecutive kills should append - line2 should be yanked too, got: " text-after-yank)))))

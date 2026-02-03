(ns lexicon.ui.buffers.messages-test
  "E2E tests for Emacs messages semantic - Epic #86

  Tests critical invariants:
  - Messages are stored in *Messages* buffer
  - Echo area shows last message"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-messages-stored-in-buffer
  (testing "Emacs invariant: Messages stored in *Messages* buffer"
    (h/setup-test*)

    ;; Trigger some commands that should produce messages
    ;; Try C-g (keyboard-quit) which should show "Quit" message
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Check echo area for a message
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains echo-text "Quit")
              (.contains echo-text "quit")
              (not (empty? echo-text)))
          (str "Echo area should show a message, got: " echo-text)))

    ;; Try to switch to *Messages* buffer
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (e/fill h/*driver* {:css ".minibuffer-input"} "*Messages*")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; *Messages* buffer should exist and have content
    (let [messages-text (h/get-buffer-text*)]
      (is (not (empty? messages-text))
          (str "*Messages* buffer should exist and contain message history, got: " messages-text)))))

(deftest test-echo-area-shows-last-message
  (testing "Emacs invariant: Echo area shows most recent message"
    (h/setup-test*)

    ;; Trigger a command - C-g for quit
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Echo area should show the quit message
    (let [echo-text-1 (h/get-echo-area-text)]
      (is (or (.contains echo-text-1 "Quit")
              (.contains echo-text-1 "quit"))
          (str "Echo area should show 'Quit' message, got: " echo-text-1)))

    ;; Trigger another command that produces a message
    ;; Opening a non-existent command with M-x should show a message
    (h/press-meta "x")
    (Thread/sleep 100)

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Echo area should update with new quit message
    (let [echo-text-2 (h/get-echo-area-text)]
      (is (or (.contains echo-text-2 "Quit")
              (.contains echo-text-2 "quit")
              (not (empty? echo-text-2)))
          "Echo area should show most recent message"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.ui.buffers.messages-test))

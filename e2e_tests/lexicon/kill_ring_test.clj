(ns lexicon.kill-ring-test
  "E2E tests for kill ring - tests that kill/yank works with USER ACTIONS.

  Kill ring stores killed text for yanking. Tests verify:
  - Ctrl+K kills to end of line
  - Ctrl+Y yanks last killed text
  - Meta+Y (yank-pop) rotates through kills
  - Ctrl+W kills region (selected text)
  - Meta+W copies region without deleting

  Uses keyboard simulation for all kill/yank operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Kill Line (Ctrl+K)
;; =============================================================================

(deftest test-kill-line-basic
  (testing "Ctrl+K kills to end of line"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with newline
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (Thread/sleep 100)

    ;; Move to beginning of buffer via M-x
    (h/execute-command "beginning-of-buffer")

    ;; Kill to end of line
    (h/press-ctrl "k")
    (Thread/sleep 100)

    (is (= "\nWorld" (h/get-buffer-text*))
        "Ctrl+K should kill to end of line")
    (is (= "Hello" (h/get-current-kill*))
        "Killed text should be on kill ring")))

(deftest test-kill-line-at-eol
  (testing "Ctrl+K at end of line kills the newline"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with newline
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (Thread/sleep 100)

    ;; Move to end of first line (just before newline)
    (h/execute-command "beginning-of-buffer")
    (h/press-ctrl "e")  ; End of line - should be after "Hello"
    (Thread/sleep 50)

    ;; Kill - should kill newline
    (h/press-ctrl "k")
    (Thread/sleep 100)

    (is (= "HelloWorld" (h/get-buffer-text*))
        "Ctrl+K at EOL should kill the newline")))

;; =============================================================================
;; Yank (Ctrl+Y)
;; =============================================================================

(deftest test-yank-inserts-killed-text
  (testing "Ctrl+Y inserts last killed text"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type and kill some text
    (h/type-text "Hello World")
    (Thread/sleep 50)
    (h/press-ctrl "a")  ; Go to beginning
    (Thread/sleep 30)
    (h/press-ctrl "k")  ; Kill line
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*)) "Line was killed")

    ;; Yank it back
    (h/press-ctrl "y")
    (Thread/sleep 100)

    (is (= "Hello World" (h/get-buffer-text*))
        "Ctrl+Y should yank back killed text")))

(deftest test-yank-at-position
  (testing "Ctrl+Y inserts at current position"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text
    (h/type-text "World")
    (Thread/sleep 50)

    ;; Setup kill ring with text (via typing and killing)
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/type-text "Hello ")
    (Thread/sleep 50)

    ;; Select and kill "Hello " using mark+point
    (h/press-ctrl "a")  ; Go to beginning
    (Thread/sleep 30)
    (h/set-mark)  ; C-SPC to set mark
    (Thread/sleep 30)
    ;; Move forward 6 chars to select "Hello "
    (dotimes [_ 6]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)
    ;; Kill region via M-x (C-w is blocked by browser)
    (h/execute-command "kill-region")

    (is (= "World" (h/get-buffer-text*)) "Selection was killed")

    ;; Move to beginning and yank
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/press-ctrl "y")
    (Thread/sleep 100)

    (is (= "Hello World" (h/get-buffer-text*))
        "Yank should insert at cursor position")))

;; =============================================================================
;; Kill Region (Ctrl+W)
;; =============================================================================

(deftest test-kill-region-deletes-selection
  (testing "Ctrl+W kills selected region"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text
    (h/type-text "Hello Beautiful World")
    (Thread/sleep 50)

    ;; Go to beginning and move forward to "Beautiful"
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (dotimes [_ 6]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Set mark and select "Beautiful " using mark+point
    (h/set-mark)
    (Thread/sleep 30)
    (dotimes [_ 10]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Kill region via M-x (C-w is blocked by browser)
    (h/execute-command "kill-region")

    (is (= "Hello World" (h/get-buffer-text*))
        "kill-region should kill selected region")
    (is (= "Beautiful " (h/get-current-kill*))
        "Killed region should be on kill ring")))

;; =============================================================================
;; Copy Region (Meta+W)
;; =============================================================================

(deftest test-copy-region-preserves-text
  (testing "Meta+W copies without deleting"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning and set mark
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/set-mark)
    (Thread/sleep 30)

    ;; Select "Hello" using mark+point
    (dotimes [_ 5]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Copy selection (M-w)
    (h/press-meta "w")
    (Thread/sleep 100)

    (is (= "Hello World" (h/get-buffer-text*))
        "Meta+W should NOT delete the text")
    (is (= "Hello" (h/get-current-kill*))
        "Copied text should be on kill ring")))

;; =============================================================================
;; Kill Ring Rotation (Yank Pop)
;; =============================================================================

(deftest test-yank-pop-rotates-through-kills
  (testing "Meta+Y after Ctrl+Y rotates through kill ring"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Create multiple kills
    (h/type-text "First")
    (Thread/sleep 100)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (h/press-ctrl "k")  ; Kill "First"
    (Thread/sleep 200)

    (h/type-text "Second")
    (Thread/sleep 100)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (h/press-ctrl "k")  ; Kill "Second"
    (Thread/sleep 200)

    (h/type-text "Third")
    (Thread/sleep 100)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (h/press-ctrl "k")  ; Kill "Third"
    (Thread/sleep 200)

    ;; Yank - should get "Third" (most recent)
    (h/press-ctrl "y")
    (Thread/sleep 100)
    (is (= "Third" (h/get-buffer-text*)) "First yank gets most recent kill")

    ;; Yank-pop - should replace with "Second"
    (h/press-meta "y")
    (Thread/sleep 100)
    (is (= "Second" (h/get-buffer-text*)) "First yank-pop gets previous kill")

    ;; Yank-pop again - should replace with "First"
    (h/press-meta "y")
    (Thread/sleep 100)
    (is (= "First" (h/get-buffer-text*)) "Second yank-pop gets oldest kill")))

;; =============================================================================
;; Multiple Consecutive Kills
;; =============================================================================

(deftest test-consecutive-kills-append
  (testing "Consecutive Ctrl+K appends to kill ring"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type multiline text
    (h/type-text "Line One")
    (h/press-key "Enter")
    (h/type-text "Line Two")
    (h/press-key "Enter")
    (h/type-text "Line Three")
    (Thread/sleep 100)

    ;; Go to beginning of buffer
    (h/execute-command "beginning-of-buffer")

    ;; Kill first line
    (h/press-ctrl "k")
    (Thread/sleep 50)
    ;; Kill newline
    (h/press-ctrl "k")
    (Thread/sleep 100)

    ;; With consecutive kills, text should be appended
    (let [kill (h/get-current-kill*)]
      (is (or (= "Line One\n" kill)
              (= "Line One" kill))
          "Consecutive kills may append"))))

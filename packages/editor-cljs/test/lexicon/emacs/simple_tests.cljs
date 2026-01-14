(ns lexicon.emacs.simple-tests
  "Emacs semantic compatibility tests derived from simple-tests.el.

  Source: /tmp/emacs-source/test/lisp/simple-tests.el (Emacs 29.4)
  Contract: docs/EMACS_COMPATIBILITY_CONTRACT.md
  Coverage: docs/EMACS_TEST_COVERAGE_MAP.md

  These tests validate Lexicon's behavioral semantics against Emacs.
  Each test is a direct translation of an Emacs test, re-expressed in
  Lexicon's test language using editor-facing APIs only.

  Phase 6.6 Step 3 - Initial batch: 5 tests"
  (:require [cljs.test :refer [deftest is testing use-fixtures async]]
            [lexicon.emacs.harness :as h :refer [with-temp-buffer
                                                   insert
                                                   goto-char
                                                   point
                                                   point-min
                                                   point-max
                                                   buffer-string
                                                   buffer-substring
                                                   buffer-substrings
                                                   forward-char
                                                   call-interactively
                                                   set-prefix-arg]]
            [lexicon.test-setup :as setup]))

;; Wait for WASM before running tests
(use-fixtures :once
  {:before (fn []
             (async done
               (-> setup/wasm-load-promise
                   (.then (fn [_]
                            (.log js/console "✅ WASM ready for emacs/simple-tests")
                            (done)))
                   (.catch (fn [e]
                             (.error js/console "❌ WASM failed in emacs/simple-tests:" e)
                             (done))))))})

;; =============================================================================
;; Test 1: count-lines
;; =============================================================================
;; Source: simple-tests.el:53-61
;; Contract: 4.1 (Buffers)
;; Priority: High

(deftest count-lines-test
  (testing "Count lines in buffer - basic cases"
    (with-temp-buffer
      ;; Empty buffer has 0 lines
      (is (= 0 (h/count-lines (point-min) (point-max)))
          "Empty buffer should have 0 lines")

      ;; Single line without newline
      (insert "foo")
      (is (= 1 (h/count-lines (point-min) (point-max)))
          "Buffer with text but no newline should count as 1 line")

      ;; Multiple lines with newlines
      (insert "\nbar\nbaz\n")
      (is (= 3 (h/count-lines (point-min) (point-max)))
          "Three newlines should create 3 lines")

      ;; Add another line
      (insert "r\n")
      (is (= 4 (h/count-lines (point-min) (point-max)))
          "Four newlines should create 4 lines"))))

;; =============================================================================
;; Test 2: newline
;; =============================================================================
;; Source: simple-tests.el:120-137
;; Contract: 2.4 (Prefix Arguments), 4.2 (Point), 4.1 (Buffers)
;; Priority: CRITICAL

(deftest newline-test
  (testing "Newline command - basic insertion and prefix args"
    ;; Helper: Create buffer with "(a b" and " c d)" with point between them
    (letfn [(dummy-buffer [body-fn]
              (with-temp-buffer
                (insert "(a b")
                ;; save-excursion equivalent: save point, insert, restore point
                (let [saved-point (point)]
                  (insert " c d)")
                  (goto-char saved-point))
                (body-fn)
                (buffer-substrings)))]

      ;; Test: newline with arg 1
      (is (= ["(a b\n" " c d)"]
             (dummy-buffer (fn []
                             (call-interactively :newline))))
          "Newline should insert newline at point and move point after it")

      ;; Test: newline with prefix arg 5 (C-u 5)
      (is (= ["(a b\n\n\n\n\n" " c d)"]
             (dummy-buffer (fn []
                             (set-prefix-arg 5)
                             (call-interactively :newline))))
          "C-u 5 newline should insert 5 newlines")

      ;; Test: newline after forward-char
      (is (= ["(a b \n" "c d)"]
             (dummy-buffer (fn []
                             (forward-char 1)
                             (call-interactively :newline))))
          "Newline after moving point should insert at new position"))))

;; =============================================================================
;; Test 3: open-line
;; =============================================================================
;; Source: simple-tests.el:171-189
;; Contract: 4.2 (Point), 4.1 (Buffers)
;; Priority: CRITICAL

(deftest open-line-test
  (testing "Open-line command - insert newline without moving point"
    (letfn [(dummy-buffer [body-fn]
              (with-temp-buffer
                (insert "(a b")
                (let [saved-point (point)]
                  (insert " c d)")
                  (goto-char saved-point))
                (body-fn)
                (buffer-substrings)))]

      ;; Test: open-line with arg 1
      (is (= ["(a b" "\n c d)"]
             (dummy-buffer (fn []
                             (call-interactively :open-line))))
          "Open-line should insert newline WITHOUT moving point")

      ;; Test: open-line with prefix arg 5 (C-u 5)
      (is (= ["(a b" "\n\n\n\n\n c d)"]
             (dummy-buffer (fn []
                             (set-prefix-arg 5)
                             (call-interactively :open-line))))
          "C-u 5 open-line should insert 5 newlines without moving point")

      ;; Test: open-line after forward-char
      (is (= ["(a b " "\nc d)"]
             (dummy-buffer (fn []
                             (forward-char 1)
                             (call-interactively :open-line))))
          "Open-line after moving point should insert at new position"))))

;; =============================================================================
;; Test 4: delete-indentation (no region)
;; =============================================================================
;; Source: simple-tests.el:278-295
;; Contract: 4.1 (Buffers), 4.2 (Point)
;; Priority: High

(deftest delete-indentation-no-region-test
  (testing "Delete-indentation - join lines without region"
    (with-temp-buffer
      (insert " first \n second \n third \n fourth ")

      ;; Without prefix argument - joins current line to previous
      (call-interactively :delete-indentation)
      (is (= [" first \n second \n third" " fourth "]
             (buffer-substrings))
          "Delete-indentation should join current line to previous")

      (call-interactively :delete-indentation)
      (is (= [" first \n second" " third fourth "]
             (buffer-substrings))
          "Second delete-indentation continues joining")

      ;; With prefix argument (C-u) - joins current line to NEXT
      (goto-char (point-min))
      (set-prefix-arg '(4))
      (call-interactively :delete-indentation)
      (is (= [" first" " second third fourth "]
             (buffer-substrings))
          "Delete-indentation with prefix should join to following line"))))

;; =============================================================================
;; Test 5: delete-indentation (blank lines)
;; =============================================================================
;; Source: simple-tests.el:307-335
;; Contract: 4.1 (Buffers), 4.2 (Point)
;; Priority: High

(deftest delete-indentation-blank-line-test
  (testing "Delete-indentation - blank line edge cases"
    (with-temp-buffer
      (insert "\n\n third \n \n \n sixth \n\n")

      ;; Without prefix argument
      (call-interactively :delete-indentation)
      (is (= ["\n\n third \n \n \n sixth \n" ""]
             (buffer-substrings))
          "Delete-indentation at end should not skip blank lines")

      (call-interactively :delete-indentation)
      (is (= ["\n\n third \n \n \n sixth" ""]
             (buffer-substrings))
          "Continue joining blank lines")

      (call-interactively :delete-indentation)
      (is (= ["\n\n third \n \n" "sixth"]
             (buffer-substrings))
          "Join even with blank lines between")

      ;; With prefix argument
      (goto-char (point-min))
      (call-interactively :delete-indentation 't) ; t = join-following
      (is (= ["" "\n third \n \nsixth"]
             (buffer-substrings))
          "Delete-indentation with arg joins to following")

      (call-interactively :delete-indentation 't)
      (is (= ["" "third \n \nsixth"]
             (buffer-substrings))
          "Continue joining forward")

      (call-interactively :delete-indentation 't)
      (is (= ["third" "\nsixth"]
             (buffer-substrings))
          "Join continues through blank lines")

      (call-interactively :delete-indentation 't)
      (is (= ["third" " sixth"]
             (buffer-substrings))
          "Final join removes whitespace"))))

;; =============================================================================
;; Missing Functions (to be implemented)
;; =============================================================================

(comment
  "Functions referenced but not yet in harness:

  1. count-lines (start end)
     - Count number of lines between positions
     - Needs implementation in harness

  2. Commands invoked via call-interactively:
     - :newline - Insert newline, move point after
     - :open-line - Insert newline, keep point before
     - :delete-indentation - Join lines, remove whitespace

  These will fail when tests run - that's expected and good.
  Failures will tell us what's missing from Lexicon.")

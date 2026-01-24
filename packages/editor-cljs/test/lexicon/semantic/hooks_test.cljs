(ns lexicon.semantic.hooks-test
  "Semantic tests for hooks system - CORE primitive for extensibility.

  Hooks allow packages to extend editor behavior without modifying core code.
  Used by: all modes, directory-local variables, package initialization.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - package ecosystem depends on this"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;; Emacs-style t for true
(def t true)

;;; =============================================================================
;;; Hooks - Core Primitive Tests
;;; =============================================================================

(deftest ^:critical hooks-add-and-run
  "CRITICAL: Hooks can be added and executed.

  Semantic Guarantee:
  - (add-hook 'hook-name function) registers callback
  - (run-hooks 'hook-name) executes all callbacks
  - Callbacks run in order they were added

  Why this matters:
  - Modes need initialization hooks
  - Packages extend behavior without modifying core
  - Directory-local variables depend on hooks

  Implementation Note:
  - Store hooks in :hooks map in db
  - Each hook is a vector of functions
  - NOT implemented yet"
  (testing "Single hook executes"
    (let [called (atom false)]
      (helpers/add-hook 'test-hook (fn [] (reset! called true)))
      (helpers/run-hooks 'test-hook)
      (is @called "Hook function was called")))

  (testing "Multiple hooks execute in order"
    (let [calls (atom [])]
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :first)))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :second)))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :third)))
      (helpers/run-hooks 'test-hook)
      (is (= [:first :second :third] @calls)
          "Hooks ran in registration order"))))

(deftest ^:critical hooks-remove
  "CRITICAL: Hooks can be removed.

  Semantic Guarantee:
  - (remove-hook 'hook-name function) unregisters callback
  - Removed hook doesn't run
  - Other hooks unaffected

  Why this matters:
  - Modes need cleanup on disable
  - Temporary hooks for one-time operations
  - Package unload must clean up

  Implementation Note:
  - Remove function from hook vector
  - Use identical? for comparison
  - NOT implemented yet"
  (testing "Removed hook doesn't execute"
    (let [called (atom false)
          hook-fn (fn [] (reset! called true))]
      (helpers/add-hook 'test-hook hook-fn)
      (helpers/remove-hook 'test-hook hook-fn)
      (helpers/run-hooks 'test-hook)
      (is (not @called) "Removed hook didn't run")))

  (testing "Other hooks still execute after removal"
    (let [calls (atom [])
          first-fn (fn [] (swap! calls conj :first))
          second-fn (fn [] (swap! calls conj :second))
          third-fn (fn [] (swap! calls conj :third))]
      (helpers/add-hook 'test-hook first-fn)
      (helpers/add-hook 'test-hook second-fn)
      (helpers/add-hook 'test-hook third-fn)
      (helpers/remove-hook 'test-hook second-fn)
      (helpers/run-hooks 'test-hook)
      (is (= [:first :third] @calls)
          "Remaining hooks still ran"))))

(deftest ^:high hooks-with-arguments
  "HIGH: Hooks can receive arguments.

  Semantic Guarantee:
  - (run-hooks 'hook-name arg1 arg2 ...) passes args to callbacks
  - All hooks receive same arguments
  - Used for change notifications

  Why this matters:
  - before-change-functions needs (beg end)
  - after-change-functions needs (beg end old-len)
  - Mode hooks may need buffer parameter

  Implementation Note:
  - Apply args to each hook function
  - NOT implemented yet"
  (testing "Hook receives arguments"
    (let [args (atom nil)]
      (helpers/add-hook 'test-hook (fn [a b] (reset! args [a b])))
      (helpers/run-hooks 'test-hook 42 "hello")
      (is (= [42 "hello"] @args)
          "Hook received correct arguments")))

  (testing "Multiple hooks receive same arguments"
    (let [calls (atom [])]
      (helpers/add-hook 'test-hook (fn [x] (swap! calls conj [:first x])))
      (helpers/add-hook 'test-hook (fn [x] (swap! calls conj [:second x])))
      (helpers/run-hooks 'test-hook 123)
      (is (= [[:first 123] [:second 123]] @calls)
          "All hooks received same argument"))))

(deftest ^:high hooks-error-handling
  "HIGH: Hook errors don't crash editor.

  Semantic Guarantee:
  - Error in one hook doesn't prevent others from running
  - Error is logged but contained
  - Editor remains stable

  Why this matters:
  - Third-party packages may have bugs
  - User shouldn't lose work from hook error
  - Debugging needs error visibility

  Implementation Note:
  - Wrap each hook in try/catch
  - Log errors to *Messages*
  - Continue with next hook
  - NOT implemented yet"
  (testing "Error in hook doesn't prevent subsequent hooks"
    (let [calls (atom [])]
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :first)))
      (helpers/add-hook 'test-hook (fn [] (throw (js/Error. "Test error"))))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :third)))
      (helpers/run-hooks 'test-hook)
      (is (= [:first :third] @calls)
          "Hooks after error still ran")))

  (testing "Error is logged but editor continues"
    (helpers/add-hook 'test-hook (fn [] (throw (js/Error. "Hook error"))))
    ;; Should not throw
    (is (nil? (helpers/run-hooks 'test-hook))
        "run-hooks completed despite error")))

(deftest ^:medium hooks-local-vs-global
  "MEDIUM: Hooks can be buffer-local or global.

  Semantic Guarantee:
  - Default: hooks are global (affect all buffers)
  - (add-hook 'hook-name fn nil t) makes hook buffer-local
  - Buffer-local hooks only run in that buffer

  Why this matters:
  - Mode-specific hooks (only in dired buffers)
  - File-local variable hooks
  - Cleanup when buffer killed

  Implementation Note:
  - Global hooks in :hooks map
  - Buffer-local hooks in buffer's :local-hooks
  - run-hooks checks both
  - NOT implemented yet"
  (testing "Global hook runs in all buffers"
    (let [calls (atom [])]
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj (helpers/buffer-name))))

      (with-test-buffer "*buf1*"
        (helpers/run-hooks 'test-hook))

      (with-test-buffer "*buf2*"
        (helpers/run-hooks 'test-hook))

      (is (= ["*buf1*" "*buf2*"] @calls)
          "Global hook ran in both buffers")))

  (testing "Buffer-local hook only runs in that buffer"
    (let [calls (atom [])]
      (with-test-buffer "*buf1*"
        (helpers/add-hook 'test-hook
                         (fn [] (swap! calls conj :buf1-hook))
                         nil t)  ; Buffer-local
        (helpers/run-hooks 'test-hook))

      (with-test-buffer "*buf2*"
        (helpers/run-hooks 'test-hook))

      (is (= [:buf1-hook] @calls)
          "Local hook only ran in its buffer"))))

;;; =============================================================================
;;; Standard Editor Hooks
;;; =============================================================================

(deftest ^:critical before-change-functions
  "CRITICAL: before-change-functions hook fires before buffer edits.

  Semantic Guarantee:
  - Runs before every insert/delete
  - Receives (beg end) of about-to-be-changed region
  - Can prevent change by throwing error
  - Runs even in read-only buffers (for detection)

  Why this matters:
  - Syntax highlighting needs change notifications
  - Modes track buffer modifications
  - Read-only enforcement

  Implementation Note:
  - Fire from WASM gap buffer before edit
  - Pass position range to hooks
  - NOT implemented yet"
  (testing "Hook fires on insert"
    (with-test-buffer "*test*"
      (let [changes (atom [])]
        (helpers/add-hook 'before-change-functions
                         (fn [beg end]
                           (swap! changes conj [:before beg end])))

        (helpers/goto-char 0)
        (helpers/insert "Hello")

        (is (= [[:before 0 0]] @changes)
            "before-change fired with insertion point"))))

  (testing "Hook fires on delete"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [changes (atom [])]
        (helpers/add-hook 'before-change-functions
                         (fn [beg end]
                           (swap! changes conj [:before beg end])))

        (helpers/delete-region 6 11)

        (is (= [[:before 6 11]] @changes)
            "before-change fired with deletion range"))))

  (testing "Hook can prevent change by throwing"
    (with-test-buffer "*test*"
      (helpers/add-hook 'before-change-functions
                       (fn [_ _] (throw (js/Error. "Change prevented"))))

      (is (thrown? js/Error (helpers/insert "Blocked"))
          "Change was prevented by hook"))))

(deftest ^:critical after-change-functions
  "CRITICAL: after-change-functions hook fires after buffer edits.

  Semantic Guarantee:
  - Runs after every insert/delete
  - Receives (beg end old-len) - range changed and old length
  - Buffer is already modified
  - Used for re-highlighting, updating state

  Why this matters:
  - Syntax highlighting updates on change
  - Modes update internal state
  - Undo system integration

  Implementation Note:
  - Fire from WASM gap buffer after edit
  - old-len is length of deleted text (0 for insert)
  - NOT implemented yet"
  (testing "Hook fires on insert with old-len 0"
    (with-test-buffer "*test*"
      (let [changes (atom [])]
        (helpers/add-hook 'after-change-functions
                         (fn [beg end old-len]
                           (swap! changes conj [:after beg end old-len])))

        (helpers/goto-char 0)
        (helpers/insert "Hello")

        (is (= [[:after 0 5 0]] @changes)
            "after-change fired with new range and old-len 0"))))

  (testing "Hook fires on delete with old-len > 0"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [changes (atom [])]
        (helpers/add-hook 'after-change-functions
                         (fn [beg end old-len]
                           (swap! changes conj [:after beg end old-len])))

        (helpers/delete-region 6 11)  ; Delete "World" (5 chars)

        (is (= [[:after 6 6 5]] @changes)
            "after-change: range collapsed, old-len is deletion size"))))

  (testing "Hook sees modified buffer"
    (with-test-buffer "*test*"
      (let [content (atom nil)]
        (helpers/add-hook 'after-change-functions
                         (fn [_ _ _]
                           (reset! content (helpers/buffer-text))))

        (helpers/insert "Modified")

        (is (= "Modified" @content)
            "Hook saw buffer after modification")))))

(deftest ^:high mode-hooks
  "HIGH: Major modes run mode hooks on activation.

  Semantic Guarantee:
  - Major mode calls (run-mode-hooks 'mode-name-hook)
  - User can customize mode behavior via hooks
  - Hooks run after mode setup, before buffer display

  Why this matters:
  - User customization point
  - Package integration (e.g., company-mode in all prog modes)
  - Local variable application

  Implementation Note:
  - Each major mode defines its hook
  - Mode activation runs hooks at end
  - NOT implemented yet"
  (testing "Dired mode hook runs on dired activation"
    (let [hook-ran (atom false)]
      (helpers/add-hook 'dired-mode-hook (fn [] (reset! hook-ran true)))
      (helpers/dired "/tmp")
      (is @hook-ran "dired-mode-hook executed")))

  (testing "Mode hook receives buffer as context"
    (let [mode-buffer (atom nil)]
      (helpers/add-hook 'fundamental-mode-hook
                       (fn [] (reset! mode-buffer (helpers/current-buffer-id))))
      (with-test-buffer "*test*"
        (is (= (helpers/current-buffer-id) @mode-buffer)
            "Hook ran in correct buffer")))))

;;; =============================================================================
;;; Integration with Core Systems
;;; =============================================================================

(deftest ^:high hooks-with-inhibit-read-only
  "HIGH: Hooks can use inhibit-read-only to modify buffers.

  Semantic Guarantee:
  - Read-only buffer blocks user edits
  - Hooks can use (let [inhibit-read-only true] ...) to edit
  - Used by modes to manage their own buffers

  Why this matters:
  - Dired updates its read-only listing
  - Help buffers add cross-references
  - Modes control their own content

  Implementation Note:
  - Hooks check inhibit-read-only dynamic var
  - NOT implemented yet"
  (testing "Hook can modify read-only buffer with inhibit"
    (with-test-buffer "*test*"
      (helpers/insert "Initial")
      (helpers/set-buffer-read-only true)

      (helpers/add-hook 'test-hook
                       (fn []
                         (let [inhibit-read-only true]
                           (helpers/insert " Modified"))))

      (helpers/run-hooks 'test-hook)

      (is (= "Initial Modified" (helpers/buffer-text))
          "Hook modified read-only buffer"))))

(deftest ^:medium hooks-cleanup-on-buffer-kill
  "MEDIUM: Buffer-local hooks cleaned up when buffer killed.

  Semantic Guarantee:
  - Buffer kill removes buffer-local hooks
  - Global hooks unaffected
  - Prevents memory leaks

  Why this matters:
  - Long-lived editor with many temp buffers
  - Package unload needs cleanup
  - Memory efficiency

  Implementation Note:
  - Buffer kill clears :local-hooks
  - NOT implemented yet"
  (testing "Buffer-local hooks removed on kill"
    (let [calls (atom 0)]
      (with-test-buffer "*temp*"
        (let [buf-id (helpers/current-buffer-id)]
          (helpers/add-hook 'test-hook (fn [] (swap! calls inc)) nil t)
          (helpers/kill-buffer buf-id)))

      ;; Hook shouldn't run after buffer killed
      (helpers/run-hooks 'test-hook)
      (is (= 0 @calls) "Buffer-local hook was cleaned up"))))

;;; =============================================================================
;;; Advanced Hook Patterns
;;; =============================================================================

(deftest ^:medium hooks-run-hook-with-args-until-success
  "MEDIUM: run-hook-with-args-until-success stops on first non-nil.

  Semantic Guarantee:
  - Runs hooks in order
  - Stops when hook returns non-nil
  - Returns that value
  - Used for query hooks (is-this-valid?)

  Why this matters:
  - Completion backends (try each until one works)
  - File handlers (which backend handles this file?)
  - Fallback chains

  Implementation Note:
  - Variant of run-hooks that checks return values
  - NOT implemented yet"
  (testing "Stops at first non-nil return"
    (let [calls (atom [])]
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :first) nil))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :second) :found))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :third) nil))

      (let [result (helpers/run-hook-with-args-until-success 'test-hook)]
        (is (= :found result) "Returned first non-nil value")
        (is (= [:first :second] @calls) "Stopped after success")))))

(deftest ^:medium hooks-run-hook-with-args-until-failure
  "MEDIUM: run-hook-with-args-until-failure stops on first nil.

  Semantic Guarantee:
  - Runs hooks in order
  - Stops when hook returns nil
  - Returns nil on failure, last value on success
  - Used for validation chains (all must pass)

  Why this matters:
  - Permission checks (all validators must agree)
  - Precondition checking
  - AND-style hook composition

  Implementation Note:
  - Variant of run-hooks that checks for nil
  - NOT implemented yet"
  (testing "Stops at first nil return"
    (let [calls (atom [])]
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :first) true))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :second) nil))
      (helpers/add-hook 'test-hook (fn [] (swap! calls conj :third) true))

      (let [result (helpers/run-hook-with-args-until-failure 'test-hook)]
        (is (nil? result) "Returned nil on failure")
        (is (= [:first :second] @calls) "Stopped after failure")))))

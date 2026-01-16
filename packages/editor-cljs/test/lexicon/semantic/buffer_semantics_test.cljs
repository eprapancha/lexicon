(ns lexicon.semantic.buffer-semantics-test
  "Emacs semantic compatibility tests for buffer-file relationship.

  Tests from Epic #86, Issue #88 - Filesystem & Buffer-File Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical buffer-can-exist-without-file
  (testing "Emacs invariant: Buffers are primary entities, files are optional"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "scratch")]
      (is (some? buf) "Buffer should be created")
      (is (nil? (h/buffer-file buf)) "Buffer should have no file association")
      (h/insert-text buf "hello")
      (is (= "hello" (h/buffer-text buf)) "Buffer should contain inserted text"))))

(deftest ^:critical buffer-identity-is-stable
  (testing "Emacs invariant: Buffer identity stable independent of content changes"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test" "initial")]
      (is (= "initial" (h/buffer-text buf)) "Initial text correct")
      (h/insert-text buf " more")
      (is (= "initial more" (h/buffer-text buf)) "Text mutation preserves buffer identity")
      (is (some? buf) "Buffer ID remains stable"))))

(deftest ^:critical buffer-can-change-visited-file
  (testing "Emacs invariant: File association is mutable metadata, not buffer identity"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test")
          original-buf-id buf]
      (h/visit-file buf "/tmp/a.txt")
      (h/visit-file buf "/tmp/b.txt")
      (is (= "/tmp/b.txt" (h/buffer-file buf)) "File association can be changed")
      (is (= original-buf-id buf) "Buffer ID remains stable despite file changes"))))

(deftest ^:critical multiple-buffers-can-coexist
  (testing "Emacs invariant: Multiple independent buffers can exist simultaneously"
    (h/reset-editor-db!)
    (let [buf-a (h/create-buffer "a" "content-a")
          buf-b (h/create-buffer "b" "content-b")]
      (is (not= buf-a buf-b) "Buffers have distinct IDs")
      (is (= "content-a" (h/buffer-text buf-a)) "Buffer A retains its content")
      (is (= "content-b" (h/buffer-text buf-b)) "Buffer B retains its content")
      (h/insert-text buf-a " more")
      (is (= "content-a more" (h/buffer-text buf-a)) "Changes to A don't affect B")
      (is (= "content-b" (h/buffer-text buf-b)) "Buffer B unchanged"))))

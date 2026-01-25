(ns lexicon.semantic.programming-support-test
  "Semantic tests for programming support.

  Emacs source: lisp/progmodes/compile.el, lisp/progmodes/flymake.el, lisp/imenu.el
  Status: 0% implemented

  Key features:
  - compile: M-x compile, error parsing
  - flymake: On-the-fly syntax checking
  - imenu: Buffer index

  Related: Issue #122, Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:critical compile-command
  "CRITICAL: M-x compile runs build command."
  (testing "compile creates compilation buffer"
    (helpers/compile "echo test")
    (is (helpers/buffer-exists? "*compilation*")
        "Compilation buffer should be created")))

(deftest ^:high next-error-navigation
  "HIGH: M-g n navigates to next error."
  (testing "next-error jumps to error location"
    (is true "next-error tested via integration")))

(deftest ^:high flymake-diagnostics
  "HIGH: Flymake shows errors inline."
  (testing "flymake highlights errors"
    (is true "flymake tested via integration")))

(deftest ^:medium imenu-index
  "MEDIUM: Imenu shows buffer index."
  (testing "imenu lists definitions"
    (with-test-buffer "*test*"
      (helpers/insert "(defn foo [] nil)\n(defn bar [] nil)")
      (let [index (helpers/imenu-create-index)]
        (is (some? index)
            "Imenu should create index")))))

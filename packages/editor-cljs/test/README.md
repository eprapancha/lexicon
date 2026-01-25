# DO NOT ADD TESTS HERE

This directory is intentionally empty. A lint rule (`bb lint:browser-tests`) enforces this.

All tests must be written in `/e2e_tests/` using Etaoin with keyboard simulation.

## Correct Pattern

```clojure
;; In e2e_tests/lexicon/my_test.clj
(ns lexicon.my-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-something
  (h/setup-test*)
  (h/clear-buffer)
  (h/type-text "hello")
  (h/press-ctrl "z")
  (is (= "" (h/get-buffer-text*))))
```

## Why?

- E2E tests simulate real user interaction (keyboard, mouse)
- No evalLisp - tests what users actually experience
- Debuggable with full Clojure tooling
- Run: `bb test:e2e my-test`

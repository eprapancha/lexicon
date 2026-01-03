# Lexicon Test Package

Minimal test package for validating SCI (Small Clojure Interpreter) integration.

## Purpose

This package is used to test:
- Package loading from local directory
- SCI evaluation of external package code
- Core API access from sandboxed environment
- Package lifecycle (initialize!/cleanup!)

## Structure

```
lexicon-test-package/
├── package.edn                 # Package metadata
├── src/
│   └── lexicon/
│       └── test_package/
│           └── core.cljs       # Entry namespace
└── README.md                   # This file
```

## Testing

Load this package to verify SCI integration:

```clojure
(require '[lexicon.packages.sci :as sci])

;; Read package source
(def source (slurp "packages/lexicon-test-package/src/lexicon/test_package/core.cljs"))

;; Load package with :external trust level (SCI sandbox)
(sci/load-package-source :lexicon-test-package source :external)

;; Execute test command
(rf/dispatch [:test-package/hello])
;; Should display: "Hello from test package! SCI integration works."
```

## Commands

- `M-x test-package/hello` - Display greeting message

## License

Part of Lexicon editor core. MIT License.

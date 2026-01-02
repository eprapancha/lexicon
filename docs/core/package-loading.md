# Lexicon Package Loading Specification

**Version:** 1.0.0
**Status:** Design Document
**Last Updated:** 2026-01-02

## Table of Contents

1. [Overview](#overview)
2. [Package Structure](#package-structure)
3. [SCI Integration](#sci-integration)
4. [Package Lifecycle](#package-lifecycle)
5. [Dependency Management](#dependency-management)
6. [Package API](#package-api)
7. [Security Model](#security-model)
8. [Implementation Guidelines](#implementation-guidelines)

---

## Overview

Lexicon's package system enables **runtime loading** of external packages written in ClojureScript. Packages extend the editor without requiring recompilation of the core.

### Why Runtime Loading?

**Without runtime loading**:
- All packages compiled into bundle
- Bundle grows with every package
- Can't add packages without rebuild
- Hard to distribute third-party packages

**With runtime loading** (SCI):
- Load packages on demand
- Small core bundle
- Users install packages dynamically
- Package marketplace possible

### Design Principles

1. **Secure**: Packages run in sandboxed environment (SCI)
2. **Compatible**: Use standard Core API (same interface)
3. **Composable**: Packages can depend on other packages
4. **Versioned**: Semantic versioning for compatibility
5. **Declarative**: Package metadata defines requirements

### Comparison to Emacs

| Feature | Emacs | Lexicon |
|---------|-------|---------|
| Package format | `.el` files | `.cljs` files |
| Loading | `load`, `require` | SCI `eval-string` |
| Package manager | `package.el` | Planned |
| Repository | MELPA, ELPA | Planned |
| Dependencies | Listed in header | `:dependencies` in metadata |
| Autoloads | `;;;###autoload` | `:autoload` in metadata |

---

## Package Structure

### Package Metadata

Every package has a **metadata file** (`.edn`):

```clojure
{:name "my-package"
 :version "1.0.0"
 :description "Does something useful"
 :author "Jane Doe <jane@example.com>"
 :url "https://github.com/jane/my-package"
 :license "MIT"

 :dependencies {"lexicon-core" "^1.0.0"
                "another-package" "^2.1.0"}

 :main "my-package.core"

 :autoload [{:command :my-package/do-thing
             :keys "C-c m t"
             :description "Do the thing"}]

 :hooks [{:hook :after-save-hook
          :function :my-package/format-on-save
          :priority 50}]}
```

### Metadata Fields

| Field | Required | Description |
|-------|----------|-------------|
| `:name` | Yes | Package name (kebab-case) |
| `:version` | Yes | Semantic version (e.g., "1.0.0") |
| `:description` | Yes | Short description |
| `:author` | No | Author name and email |
| `:url` | No | Homepage URL |
| `:license` | No | License (e.g., "MIT", "GPL-3.0") |
| `:dependencies` | No | Map of package → version spec |
| `:main` | Yes | Main namespace to load |
| `:autoload` | No | Commands to autoload |
| `:hooks` | No | Hooks to register on load |

### Package Directory Structure

```
my-package/
├── package.edn              # Metadata
├── src/
│   └── my_package/
│       ├── core.cljs        # Main namespace (:main)
│       ├── commands.cljs    # Command definitions
│       └── utils.cljs       # Utilities
├── test/
│   └── my_package/
│       └── core_test.cljs
└── README.md
```

### Main Namespace

The `:main` namespace defines package initialization:

```clojure
(ns my-package.core
  (:require [lexicon.core.api :as api]))

(defn initialize! []
  "Called when package is loaded"
  (api/define-command :my-package/hello
    {:interactive true
     :doc "Say hello"}
    (fn [] (api/message "Hello from my-package!")))

  (api/define-key :global-map "C-c m h" :my-package/hello))

(defn shutdown! []
  "Called when package is unloaded"
  (api/unregister-command :my-package/hello))
```

---

## SCI Integration

### What is SCI?

**SCI** (Small Clojure Interpreter) is a ClojureScript interpreter that evaluates Clojure code at runtime:

```clojure
(require '[sci.core :as sci])

(sci/eval-string "(+ 1 2 3)")  ;; => 6
```

### Why SCI for Lexicon?

1. **Runtime evaluation**: Load packages without recompiling
2. **Sandboxing**: Restrict package access to allowed APIs only
3. **ClojureScript native**: Packages are real CLJS code
4. **Performance**: Compiled to JS (not interpreted)

### SCI Context

Packages run in an **SCI context** with access to Core API:

```clojure
(def package-context
  (sci/init
    {:namespaces
     {'lexicon.core.api
      {'define-command api/define-command
       'define-key api/define-key
       'insert! api/insert!
       'point api/point
       ;; ... all Core API functions
       }}

     :bindings
     {'*current-buffer* (sci/new-dynamic-var '*current-buffer*)
      '*prefix-arg* (sci/new-dynamic-var '*prefix-arg*)
      ;; ... dynamic context vars
      }}))
```

### Loading a Package with SCI

```clojure
(defn load-package [package-name]
  (let [package-dir (str "packages/" package-name "/")
        metadata (load-edn (str package-dir "package.edn"))
        main-file (str package-dir "src/"
                       (clojure.string/replace (:main metadata) "." "/")
                       ".cljs")
        source (slurp main-file)]

    ;; Evaluate package source in SCI context
    (sci/eval-string source {:env package-context})

    ;; Call initialize!
    (sci/eval-string "(initialize!)" {:env package-context})

    ;; Register package
    (register-package package-name metadata)))
```

---

## Package Lifecycle

### Lifecycle Stages

```
1. Discovery
   ↓
2. Dependency Resolution
   ↓
3. Download/Fetch
   ↓
4. Validation
   ↓
5. Loading (SCI evaluation)
   ↓
6. Initialization (call initialize!)
   ↓
7. Active (package running)
   ↓
8. Unloading (call shutdown!)
   ↓
9. Cleanup
```

### Stage Details

#### 1. Discovery

Find available packages:
- Local directory scan
- Remote registry query (future)

```clojure
(discover-packages "~/.lexicon/packages/")
;; => [{:name "my-package" :version "1.0.0" ...}
;;     {:name "another-package" :version "2.1.0" ...}]
```

#### 2. Dependency Resolution

Resolve dependency graph:

```clojure
;; User wants: vertico 1.0.0
;; vertico depends on: orderless ^1.0.0
;; → Install: vertico 1.0.0, orderless 1.1.0 (latest matching ^1.0.0)
```

**Algorithm**: Topological sort with version constraint satisfaction

#### 3. Download/Fetch

Retrieve package files:
- Local: Copy from directory
- Remote: Fetch from GitHub/registry (future)

```clojure
(fetch-package "vertico" "1.0.0")
;; Downloads to: ~/.lexicon/packages/vertico-1.0.0/
```

#### 4. Validation

Verify package integrity:
- Checksum validation (future)
- Metadata schema validation
- Core API version compatibility check

```clojure
(validate-package package-metadata)
;; Checks:
;; - Required fields present
;; - Version format valid
;; - Dependencies satisfiable
;; - Core API version compatible
```

#### 5. Loading (SCI Evaluation)

Evaluate package source in SCI:

```clojure
(load-package-source package-name source)
;; Evaluates all .cljs files in SCI context
;; Defines functions, creates namespaces
```

#### 6. Initialization

Call package's `initialize!` function:

```clojure
(initialize-package package-name)
;; Calls (ns-name/initialize!)
;; Package registers commands, hooks, keymaps
```

#### 7. Active

Package is loaded and running:
- Commands available
- Hooks active
- Keybindings installed

#### 8. Unloading

Call package's `shutdown!` function:

```clojure
(unload-package package-name)
;; Calls (ns-name/shutdown!)
;; Package unregisters commands, hooks, keymaps
```

#### 9. Cleanup

Remove package from memory:
- Delete SCI namespace
- Remove from package registry
- GC package objects

---

## Dependency Management

### Version Specifications

Use **semver** ranges:

| Spec | Meaning | Example Matches |
|------|---------|-----------------|
| `"1.0.0"` | Exact version | 1.0.0 only |
| `"^1.0.0"` | Compatible (minor/patch) | 1.0.0, 1.1.0, 1.9.9 (not 2.0.0) |
| `"~1.0.0"` | Patch updates only | 1.0.0, 1.0.1, 1.0.9 (not 1.1.0) |
| `">= 1.0.0"` | Greater or equal | 1.0.0, 1.5.0, 2.0.0 |
| `"1.0.0 - 2.0.0"` | Range | 1.0.0 through 2.0.0 |

### Dependency Graph

Example:

```
vertico 1.0.0
  ├─→ orderless ^1.0.0
  └─→ consult ^0.30.0
        └─→ compat ^29.1.0

evil-mode 1.15.0
  └─→ goto-chg ^1.7.0
```

### Conflict Resolution

If multiple packages depend on different versions:

```clojure
;; Package A depends on: common-lib ^1.0.0
;; Package B depends on: common-lib ^1.2.0

;; Resolution: Install common-lib 1.2.0 (satisfies both ^1.0.0 and ^1.2.0)
```

**Conflict example**:
```clojure
;; Package A depends on: common-lib ^1.0.0
;; Package B depends on: common-lib ^2.0.0

;; Resolution: ERROR - Cannot satisfy both constraints
;; User must choose to disable one package or upgrade A
```

---

## Package API

### `load-package`

Loads a package by name.

**Signature**:
```clojure
(load-package package-name)
(load-package package-name version)
```

**Parameters**:
- `package-name` (string): Package to load
- `version` (optional): Specific version (default: latest)

**Returns**: Package metadata

**Example**:
```clojure
(load-package "vertico")
(load-package "vertico" "1.0.0")
```

---

### `unload-package`

Unloads a package.

**Signature**:
```clojure
(unload-package package-name)
```

**Returns**: `true` if unloaded

**Example**:
```clojure
(unload-package "vertico")
```

---

### `list-packages`

Returns all available packages.

**Signature**:
```clojure
(list-packages)
(list-packages filter)
```

**Parameters**:
- `filter` (optional): Filter predicate

**Returns**: Vector of package metadata

**Example**:
```clojure
(list-packages)
;; => [{:name "vertico" :version "1.0.0" :loaded? true}
;;     {:name "evil-mode" :version "1.15.0" :loaded? false}]

(list-packages :loaded?)
;; => [{:name "vertico" :version "1.0.0" :loaded? true}]
```

---

### `package-installed?`

Checks if package is installed.

**Signature**:
```clojure
(package-installed? package-name)
```

**Returns**: `true` if installed

---

### `package-loaded?`

Checks if package is currently loaded.

**Signature**:
```clojure
(package-loaded? package-name)
```

**Returns**: `true` if loaded

---

### `install-package`

Installs a package from registry (future).

**Signature**:
```clojure
(install-package package-name)
(install-package package-name version)
```

**Example**:
```clojure
(install-package "magit")  ;; Downloads and installs
```

---

### `uninstall-package`

Removes an installed package.

**Signature**:
```clojure
(uninstall-package package-name)
```

**Example**:
```clojure
(uninstall-package "old-package")  ;; Deletes from disk
```

---

### `update-package`

Updates a package to latest version (future).

**Signature**:
```clojure
(update-package package-name)
```

---

## Security Model

### Sandbox Restrictions

Packages run in **restricted SCI context**:

**Allowed**:
- All Core API functions
- Pure ClojureScript functions (map, filter, reduce, etc.)
- ClojureScript standard library

**Disallowed**:
- Direct DOM manipulation (use Core API instead)
- `js/eval`, `js/Function`
- Network requests (future: controlled API)
- File system access (future: controlled API)
- Global state mutation (outside package scope)

### Example Sandbox Config

```clojure
(def package-sandbox
  (sci/init
    {:namespaces
     {'lexicon.core.api {...}}  ;; Allowed

     :deny
     ['js/eval
      'js/Function
      'js/XMLHttpRequest
      'js/fetch]

     :allow-unrestricted-eval false}))
```

### Security Levels (Future)

| Level | Description | Restrictions |
|-------|-------------|--------------|
| **Trusted** | Core-blessed packages | Full Core API access |
| **Verified** | Community-reviewed | Core API + controlled network |
| **Untrusted** | User-installed | Core API only, no network |

---

## Implementation Guidelines

### For Core Developers

#### 1. SCI Context Setup

```clojure
(require '[sci.core :as sci])

(defonce package-sci-ctx
  (sci/init
    {:namespaces
     {'lexicon.core.api
      (sci/copy-ns lexicon.core.api (sci/create-ns 'lexicon.core.api))}

     :bindings
     {'*current-buffer* (sci/new-dynamic-var '*current-buffer* nil)
      '*prefix-arg* (sci/new-dynamic-var '*prefix-arg* nil)}}))
```

#### 2. Package Loading

```clojure
(defn load-package! [package-name]
  (let [metadata (load-package-metadata package-name)
        source (load-package-source package-name (:main metadata))]

    ;; Evaluate in SCI
    (sci/eval-string source {:env package-sci-ctx})

    ;; Initialize
    (sci/eval-string
      (str "(" (:main metadata) "/initialize!)")
      {:env package-sci-ctx})

    ;; Register
    (swap! loaded-packages assoc package-name metadata)))
```

#### 3. Dependency Resolution

```clojure
(defn resolve-dependencies [package-name]
  (let [metadata (load-package-metadata package-name)
        deps (:dependencies metadata)]

    ;; Topological sort
    (loop [to-load (keys deps)
           loaded #{}]
      (if (empty? to-load)
        loaded
        (let [pkg (first to-load)
              pkg-deps (get-package-dependencies pkg)
              ready? (every? loaded pkg-deps)]
          (if ready?
            (do
              (load-package! pkg)
              (recur (rest to-load) (conj loaded pkg)))
            (recur (concat (rest to-load) [pkg]) loaded)))))))
```

---

### For Package Developers

#### 1. Package Template

```clojure
;; package.edn
{:name "my-package"
 :version "1.0.0"
 :description "My awesome package"
 :dependencies {"lexicon-core" "^1.0.0"}
 :main "my-package.core"}

;; src/my_package/core.cljs
(ns my-package.core
  (:require [lexicon.core.api :as api]))

(defn initialize! []
  (api/define-command :my-package/hello
    {:interactive true :doc "Say hello"}
    (fn [] (api/message "Hello!"))))

(defn shutdown! []
  (api/unregister-command :my-package/hello))
```

#### 2. Using Dependencies

```clojure
;; package.edn
{:dependencies {"orderless" "^1.0.0"}}

;; core.cljs
(ns my-package.core
  (:require [lexicon.core.api :as api]
            [orderless.core :as orderless]))  ;; Use dependency

(defn my-completion []
  (orderless/filter-candidates candidates pattern))
```

#### 3. Autoloading

```clojure
;; package.edn
{:autoload [{:command :my-package/quick-action
             :keys "C-c m q"
             :description "Quick action"}]}

;; Lexicon registers keybinding that lazy-loads package on first use
```

---

## Migration from Current Implementation

### Current State (Phase 6)

Lexicon currently:
- No runtime package loading
- All code compiled into bundle
- No external package support

### Migration Path (Phase 7.7 - SCI Integration)

**Week 1: SCI Setup**
1. Add SCI dependency
2. Create SCI context with Core API
3. Test basic evaluation
4. Implement namespace isolation

**Week 2: Package Loader**
1. Design package metadata format
2. Implement package discovery
3. Implement loading mechanism
4. Add package registry

**Week 3: Dependency Resolution**
1. Implement semver parser
2. Implement dependency graph resolution
3. Add topological sort
4. Handle conflicts

**Week 4: Testing & Integration**
1. Create test packages
2. Test Vertico/Orderless/Consult loading
3. Performance benchmarks
4. Security audit

---

## Future Enhancements

### 1. Package Registry (Phase 8+)

Central repository:
- Search packages: `M-x package-list-packages`
- Install from registry: `M-x package-install`
- Auto-updates

### 2. Package Development Tools

```clojure
;; Package scaffolding
(create-package "my-new-package")
;; Creates template with package.edn, core.cljs, tests

;; Package testing
(test-package "my-package")
;; Runs tests in isolated environment

;; Package publishing
(publish-package "my-package")
;; Uploads to registry
```

### 3. Package Marketplace

Web UI for browsing packages:
- Screenshots
- README preview
- Download counts
- Ratings/reviews

### 4. Native Modules

Support WASM-based packages:
```clojure
{:native-modules ["tree-sitter-clojure.wasm"]}
```

---

## References

1. **SCI (Small Clojure Interpreter)**: [GitHub](https://github.com/babashka/sci)
2. **Emacs Package System**: [package.el](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html)
3. **MELPA**: [Emacs Package Archive](https://melpa.org/)
4. **Lexicon Core API**: [docs/core/core-api.md](./core-api.md)

---

## Appendix A: Example Package - Vertico

### package.edn

```clojure
{:name "vertico"
 :version "1.0.0"
 :description "VERTical Interactive COmpletion"
 :author "Daniel Mendler <mail@daniel-mendler.de>"
 :url "https://github.com/minad/vertico"
 :license "GPL-3.0"

 :dependencies {"lexicon-core" "^1.0.0"}

 :main "vertico.core"

 :autoload [{:command :vertico-mode
             :description "Toggle Vertico completion"}]}
```

### src/vertico/core.cljs

```clojure
(ns vertico.core
  (:require [lexicon.core.api :as api]))

(defn- vertico-display [candidates]
  (api/with-current-buffer "*Completions*"
    (api/erase-buffer)
    (doseq [candidate candidates]
      (api/insert! (str candidate "\n")))))

(defn- vertico-read [prompt candidates]
  (api/minibuffer-read prompt
    :completion-fn (fn [input]
                     (filter #(clojure.string/includes? % input)
                             candidates))
    :display-fn vertico-display))

(defn initialize! []
  ;; Override default completion
  (api/set-completion-function! vertico-read)

  (api/message "Vertico mode enabled"))

(defn shutdown! []
  (api/set-completion-function! nil)
  (api/message "Vertico mode disabled"))
```

---

## Appendix B: Security Considerations

### Threat Model

**Threats**:
1. Malicious packages accessing user data
2. Packages executing arbitrary JS
3. Network exfiltration of buffer contents
4. DOM manipulation breaking editor

**Mitigations**:
1. SCI sandboxing (no direct JS access)
2. Whitelist Core API functions only
3. No network access (future: controlled API)
4. No direct DOM access (use Core API)

### Code Review Process (Future)

1. **Automated checks**: Static analysis
2. **Community review**: Public code review
3. **Trust levels**: Trusted/Verified/Untrusted badges
4. **Revocation**: Ability to ban malicious packages

---

**End of Package Loading Specification**

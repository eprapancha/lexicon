**ARCHIVED 2026-01-03**

**This document has been integrated into the main ROADMAP.md:**
- Phase 0-1 → Phase 7.7: Package System Foundation & SCI Integration
- Phase 2 → Phase 7.7: Trust Model & Safety
- Phase 3 → Phase 8.1: Package Lifecycle & Startup Loading
- Phase 4 → Phase 10: Public Package Index
- Phase 5 → Phase 11: Package Developer Experience
- Phase 6 → Phase 12: Package Observability & Hygiene
- Phase 7 → Phase 13: Package Ecosystem Governance

**Preserved here for reference. See ROADMAP.md for the canonical plan.**

* * *

Lexicon Package Ecosystem — Detailed Execution Plan
===================================================

Goal
----

Establish a **public, extensible, Emacs-inspired package ecosystem** for Lexicon that:

*   allows runtime loading of packages
*   preserves serialized command semantics
*   supports arbitrary CLJS execution safely
*   scales socially before it scales technically
*   avoids premature infrastructure complexity

* * *

Guiding Principles (Non-Negotiable)
-----------------------------------

*   Packages are **code, not plugins**
*   Distribution is **git-first**, not registry-first
*   Trust is explicit, not implicit
*   Evaluation model mirrors Emacs (single-threaded, inspectable)
*   Tooling remains simple until complexity is unavoidable

* * *

Phase 0 — Foundations (Documentation & Alignment)
-------------------------------------------------

### Objectives

Establish shared understanding before code exists.

### Tasks

*    Add `docs/ARCHITECTURE_STATUS.md` (already planned)
*    Add a short section: “Extensibility & Packages (Planned)”
*    Define _what counts_ as a package vs core extension
*    Decide naming conventions (`lexicon-*`)

### Deliverables

*   Architecture Status updated
*   No runtime code yet

### Exit Criteria

*   No ambiguity about what a “Lexicon package” is

* * *

Phase 1 — Package Definition (Local, Manual)
--------------------------------------------

### Objectives

Define **what a package looks like** without solving distribution.

### Tasks

#### 1.1 Package Metadata Schema

*    Define `lexicon.edn` schema
*    Decide mandatory vs optional keys

Example (v0):

```edn
{:name "lexicon-vim"
 :version "0.1.0"
 :description "Vim-style modal editing"
 :entry lexicon.vim.core
 :lexicon-version ">=0.1.0"
 :dependencies []}
```

#### 1.2 Package Entry Semantics

*    Define what happens when `:entry` is evaluated
*    Decide:
    *   allowed side effects
    *   how commands/modes/hooks are registered

#### 1.3 Local Loading Mechanism

*    Load a package from a local directory
*    Manually evaluate its entry namespace
*    Ensure no implicit global mutation outside commands

### Deliverables

*   `lexicon.edn` spec
*   Manual local package loading
*   One example internal package

### Exit Criteria

*   A package can be loaded _by hand_ and works

* * *

Phase 2 — Runtime Evaluation & Safety Model
-------------------------------------------

### Objectives

Enable **arbitrary CLJS execution** without compromising safety.

### Tasks

#### 2.1 Evaluation Strategy

*    Decide between:
    *   raw `eval`
    *   SCI interpreter
    *   hybrid (trusted vs untrusted)

Strong recommendation:

*   SCI for third-party packages
*   Native eval for core / trusted packages

#### 2.2 Trust Model

*    Define trust levels:
    *   core
    *   local
    *   external
*    Decide what each level can do:
    *   filesystem access
    *   network
    *   editor internals

#### 2.3 Error Containment

*    Package load failures must not crash the editor
*    Errors must be observable and debuggable

### Deliverables

*   Runtime evaluation boundary
*   Documented trust levels

### Exit Criteria

*   Arbitrary CLJS can run safely in-editor

* * *

Phase 3 — Package Lifecycle & Startup Loading
---------------------------------------------

### Objectives

Make packages persist across restarts.

### Tasks

#### 3.1 Package Registry (Local)

*    Define local package state storage (EDN)
*    Track:
    *   installed packages
    *   enabled packages
    *   versions / commit hashes

#### 3.2 Startup Flow

*    Load enabled packages at startup
*    Define order guarantees
*    Ensure deterministic behavior

#### 3.3 Enable / Disable Semantics

*    Enable without reinstall
*    Disable without deletion
*    Reload for development

### Deliverables

*   Package persistence
*   Startup activation logic

### Exit Criteria

*   Packages survive restarts predictably

* * *

Phase 4 — Public Package Index (MELPA Equivalent)
-------------------------------------------------

### Objectives

Create a **public, git-based package index**.

### Tasks

#### 4.1 Index Repository

*    Create `lexicon-packages-index`
*    Define `packages.edn` format
*    Decide curation policy (initially manual)

Example:

```edn
{:packages
 [{:name "lexicon-vim"
   :repo "https://github.com/lexicon-packages/lexicon-vim"
   :branch "main"
   :status :experimental}]}
```

#### 4.2 Fetch & Install

*    Fetch index
*    Clone or download package repos
*    Validate metadata

#### 4.3 Version Pinning (Minimal)

*    Record commit hash on install
*    Avoid dependency resolution initially

### Deliverables

*   Working public index
*   Basic installer

### Exit Criteria

*   Third-party packages installable from the internet

* * *

Phase 5 — Developer & Author Experience
---------------------------------------

### Objectives

Make package development **pleasant and idiomatic**.

### Tasks

#### 5.1 Package Author Guide

*    Structure of a package
*    How to define commands
*    How to hook into editor lifecycle
*    Testing expectations

#### 5.2 Tooling

*    babashka tasks for:
    *   package validation
    *   local reload
*    Clear error messages

#### 5.3 Testing Expectations

*    Define minimal test requirements
*    Encourage regression tests
*    Provide examples

### Deliverables

*   `docs/PACKAGES.md`
*   Example reference packages

### Exit Criteria

*   External contributors can build packages without hand-holding

* * *

Phase 6 — Observability, Debugging, and Hygiene
-----------------------------------------------

### Objectives

Prevent ecosystem entropy.

### Tasks

*    Structured logging for:
    *   package load
    *   evaluation
    *   failures
*    Warnings for deprecated APIs
*    Track performance impact of packages
*    Clear debugging workflow

### Deliverables

*   Observable package lifecycle
*   Debugging playbook

### Exit Criteria

*   Package issues are diagnosable, not mysterious

* * *

Phase 7 — Governance & Social Scaling (Later)
---------------------------------------------

### Objectives

Scale _people_, not infrastructure.

### Tasks

*    Define contribution guidelines for packages
*    Add status signals (`stable`, `experimental`, `unmaintained`)
*    Minimal CI for index validation
*    No hard gatekeeping

### Deliverables

*   Sustainable ecosystem norms

### Exit Criteria

*   Community packages grow without core friction

* * *

Tracking Recommendation
-----------------------

Track this plan using:

*   GitHub issues (one issue per task cluster)
*   Labels:
    *   `packages`
    *   `architecture`
    *   `security`
    *   `docs`
*   Milestones per phase


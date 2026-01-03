# **Elisp Compatibility for Lexicon**

### **A Feasibility Study and Phased Execution Plan**

## **Status**

**Exploratory but strategically justified**

---

## **Abstract**

Lexicon aims to be a browser-native, Emacs-inspired programmable editor. A major risk to ecosystem adoption is the impracticality of reimplementing decades of mature Emacs packages (e.g. consult, embark, orderless) from scratch.

This document proposes a **restricted Elisp compatibility layer**, built on top of **SCI**, that enables partial reuse of existing Elisp package source code. The goal is **not full Emacs compatibility**, but **selective reuse of high-value logic** while reimplementing UI and editor-specific surfaces natively in Lexicon.

This approach prioritizes:

* incremental feasibility  
* architectural containment  
* early wins  
* explicit non-goals

---

## **1\. Problem Statement**

### **1.1 The Ecosystem Gap**

Emacs’ power derives not from the editor core alone, but from:

* thousands of packages  
* decades of accumulated logic  
* deeply refined workflows

Reimplementing complex packages (e.g. consult, embark) in CLJS is:

* time-prohibitive  
* error-prone  
* likely to diverge functionally

### **1.2 Core Question**

Can Lexicon reuse meaningful portions of Elisp package logic **without** reimplementing Emacs?

---

## **2\. Non-Goals (Critical to State Explicitly)**

This project **does not** aim to:

* run arbitrary Emacs packages unmodified  
* implement Emacs bytecode  
* replicate Emacs C internals  
* support overlays, text properties, redisplay hooks  
* provide drop-in compatibility

Any plan assuming these is infeasible.

---

## **3\. Proposed Solution Overview**

### **3.1 Conceptual Model**

A **restricted Elisp runtime**, layered on top of SCI:

Elisp source  
   ↓  
Elisp reader \+ AST  
   ↓  
Restricted Elisp evaluator  
   ↓  
SCI runtime  
   ↓  
Lexicon host APIs

This is a **compatibility layer**, not an interpreter in the traditional sense.

Comparable systems:

* Wine (Win32 compatibility)  
* Neovim Lua shims  
* VSCode extension host abstractions

---

## **4\. Why SCI Is the Right Substrate**

SCI already provides:

* sandboxed evaluation  
* dynamic vars  
* controlled global environment  
* host interop  
* error isolation

What it lacks—but can be added:

* Elisp reader  
* Elisp macro semantics (subset)  
* Elisp-specific dynamic scoping rules

SCI handles *execution*; Lexicon defines *semantics*.

---

## **5\. Scope of Elisp Support (Explicit Contract)**

### **5.1 Language Features (Supported)**

| Feature | Status |
| ----- | ----- |
| Symbols | ✅ |
| Lists | ✅ |
| Alists / Plists | ✅ |
| `defun`, `lambda` | ✅ |
| `let`, `let*` | ✅ |
| Dynamic variables | ✅ |
| Function values | ✅ |
| `cond`, `when`, `unless` | ✅ |
| Simple macros | ⚠️ limited |

### **5.2 Language Features (Not Supported)**

| Feature | Reason |
| ----- | ----- |
| Bytecode | Emacs-specific |
| Advice system | Global mutation |
| Overlays | Buffer internals |
| Redisplay hooks | UI coupling |
| Text properties | Incompatible model |

---

## **6\. Primitive Rebinding Strategy**

Elisp primitives are **not implemented**, but **mapped**.

Example mappings:

| Elisp | Lexicon |
| ----- | ----- |
| `message` | Lexicon logging |
| `current-buffer` | Dynamic var |
| `add-hook` | Lexicon hooks |
| `run-hooks` | Lexicon hooks |
| `completing-read` | Lexicon minibuffer |
| `keymap-set` | Lexicon keymap API |

This makes **behavior explicit and auditable**.

---

## **7\. Phased Execution Plan**

Each phase is independently valuable.

---

## **Phase 0 — Preconditions (Very Important)**

### **Objective**

Ensure Lexicon is ready to host this work.

### **Required Before Starting**

* Stable command execution model  
* Dynamic scoping in CLJS finalized  
* Package loading infrastructure (even minimal)

### **Exit Criteria**

* Core editor semantics no longer shifting weekly

⚠️ **Do not start Elisp work before this phase completes**

---

## **Phase 1 — Elisp Reader & AST**

### **Objective**

Parse Elisp source into a usable AST.

### **Tasks**

* Implement Elisp reader  
* Support symbols, lists, quoting  
* Normalize AST

### **Deliverables**

* AST for trivial Elisp forms

### **Success Criteria**

(message "hello")

parses correctly

---

## **Phase 2 — Minimal Evaluator**

### **Objective**

Evaluate simple Elisp expressions.

### **Tasks**

* Implement evaluator for:  
  * function calls  
  * `defun`  
  * `let`  
* Bind Elisp environment to SCI

### **Deliverables**

* Working evaluator for basic forms

### **Success Criteria**

(defun foo () (message "hi"))  
(foo)

works

---

## **Phase 3 — Dynamic Scope & Hooks**

### **Objective**

Support Emacs-style extensibility patterns.

### **Tasks**

* Implement dynamic variable binding  
* Implement hooks (`add-hook`, `run-hooks`)  
* Ensure isolation between packages

### **Deliverables**

* Hook-driven behavior works

### **Success Criteria**

* Simple minor-mode–style package loads

---

## **Phase 4 — Primitive Coverage Expansion**

### **Objective**

Enable real package logic.

### **Tasks**

* Map core Elisp primitives  
* Introduce compatibility shims  
* Document supported API surface

### **Deliverables**

* Stable compatibility layer v0

---

## **Phase 5 — First Real Package Target**

### **Objective**

Prove real-world reuse.

### **Recommended Targets**

* `dash`  
* `s`  
* `orderless`

These are:

* logic-heavy  
* UI-light  
* widely used

### **Success Criteria**

* Real Elisp package runs unmodified or near-unmodified

---

## **Phase 6 — Partial Consult / Embark**

### **Objective**

Test high-value, high-complexity reuse.

### **Strategy**

* Reuse filtering / matching logic  
* Replace UI and minibuffer APIs  
* Keep behavior, replace surface

### **Success Criteria**

* Core workflows feel familiar  
* UI is native Lexicon

---

## **8\. Risk Analysis**

### **Technical Risks**

* Macro expansion complexity  
* Semantic mismatches  
* Performance

### **Mitigations**

* Keep macro support minimal  
* Prefer rewriting surface layers  
* Profile only after correctness

---

## **9\. When Should This Be Implemented?**

### **Best Phase: AFTER package loading, BEFORE ecosystem growth**

**Too early**:

* architecture still fluid  
* primitives unstable

**Too late**:

* ecosystem diverges  
* duplication debt accumulates

### **Ideal Trigger Conditions**

* Packages can be loaded dynamically  
* Hooks and commands are stable  
* SCI already in use

📌 **Recommended window**:

Shortly after Phase 1–2 of the Lexicon package system plan

---

## **10\. Strategic Value Assessment**

### **Costs**

* Significant engineering effort  
* Ongoing compatibility maintenance

### **Benefits**

* Massive ecosystem bootstrap  
* Faster user adoption  
* Avoids reimplementing decades of logic

### **Verdict**

**High-risk, high-reward — but uniquely aligned with Lexicon’s goals**

---

## **11\. Final Position**

This project should be treated as:

* a **parallel track**  
* tightly scoped  
* explicitly incomplete  
* strategically optional but potentially transformative

If done carefully, it could become Lexicon’s defining differentiator.
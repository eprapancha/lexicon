Excellent. This is exactly the right moment to **freeze intent into structure**.

Below is a **full design specification \+ primitive mapping \+ phased roadmap (v0 → vN)** for the Lexicon minibuffer, explicitly informed by Emacs’ lived behavior, *not* by superficial UI imitation.

This is written as a **design spec** you can drop directly into your repo and evolve over time.

---

# **Lexicon Minibuffer**

## **Design Specification & Incremental Implementation Plan**

This document defines the **conceptual model, primitives, and phased roadmap**  
for implementing the Lexicon minibuffer.

The goal is **behavioral parity with Emacs**, not visual mimicry.  
The minibuffer is treated as a **control plane**, not a widget.

---

## **1\. Design Goals (Non-Negotiable)**

1. The minibuffer is a **buffer**  
2. It is **recursive by design**  
3. It can **expand and contract dynamically**  
4. It supports **speculative interaction** (preview without commit)  
5. Completion, preview, and actions are **pluggable**  
6. Window layout is **always restored safely**  
7. Theming and modes drive behavior, not UI widgets

If a future change violates one of these, it is a design regression.

---

## **2\. Conceptual Model**

### **2.1 Core Entities**

| Concept | Description |
| ----- | ----- |
| **Minibuffer Buffer** | A normal buffer with special lifecycle rules |
| **Minibuffer Window** | A window reserved for minibuffer display |
| **Minibuffer Session** | One invocation of minibuffer interaction |
| **Minibuffer Stack** | Supports recursive minibuffers |
| **Completion Engine** | Pluggable provider of candidates |
| **Preview Function** | Side-effectful but reversible behavior |
| **Action Dispatcher** | Executes final command |

---

### **2.2 High-Level Flow**

Command → Minibuffer Session  
         → User Input  
         → Completion (optional)  
         → Preview (optional)  
         → Accept / Abort  
         → Unwind & Restore

This flow must be **purely command-driven**.

---

## **3\. Primitive Mapping (Emacs → Lexicon)**

### **3.1 Minibuffer Primitives**

| Emacs Primitive | Lexicon Primitive |
| ----- | ----- |
| `minibufferp` | `minibuffer?` |
| `read-from-minibuffer` | `minibuffer/read` |
| `completing-read` | `minibuffer/complete` |
| `enable-recursive-minibuffers` | `:minibuffer/recursive?` |
| `minibuffer-setup-hook` | `:minibuffer/on-enter` |
| `minibuffer-exit-hook` | `:minibuffer/on-exit` |
| `keyboard-quit` | `minibuffer/abort` |

---

### **3.2 Window & Layout Primitives**

| Emacs | Lexicon |
| ----- | ----- |
| `minibuffer-window` | `ui/minibuffer-window` |
| `resize-mini-windows` | `ui/minibuffer-resize-policy` |
| `save-window-excursion` | `ui/with-layout-snapshot` |

---

### **3.3 Completion & Preview**

| Emacs | Lexicon |
| ----- | ----- |
| `completion-at-point` | `completion/at-point` |
| `completion-styles` | `completion/strategies` |
| `consult-preview-at-point` | `preview/fn` |

---

### **3.4 Modeline & Theming**

| Emacs | Lexicon |
| ----- | ----- |
| Faces | Style descriptors |
| Minor modes | Buffer-local flags |
| Mode-line variables | Modeline context |

---

## **4\. Architecture Constraints (Important)**

* Minibuffer **must not** directly mutate editor state  
* All state changes occur via **commands**  
* Preview must be **undoable or discardable**  
* Recursive minibuffers must restore **exact layout**  
* No global singleton state

---

## **5\. Versioned Roadmap (v0 → vN)**

This is where we go slow and win.

---

## **v0 — Minimal Control Plane (Foundation)**

**Goal**: Make `M-x` possible without embarrassment

### **Capabilities**

* Single-line minibuffer  
* Prompt \+ input  
* Accept / abort  
* No completion  
* No preview  
* No recursion

### **Required Primitives**

* `minibuffer/read`  
* `minibuffer/accept`  
* `minibuffer/abort`  
* `ui/show-minibuffer`  
* `ui/hide-minibuffer`

### **Success Criteria**

* `M-x` works  
* `C-g` aborts cleanly  
* Layout always restores

---

## **v1 — Completion as a Service**

**Goal**: Make minibuffer useful, not fancy

### **Capabilities**

* TAB completion  
* Completion list in separate window  
* Static height minibuffer  
* Completion via pluggable engine

### **Required Additions**

* `completion/engine`  
* `completion/candidates`  
* `ui/show-completions-window`

### **Success Criteria**

* `switch-to-buffer`  
* `find-file`  
* Completion works without flicker

---

## **v2 — Expanded Minibuffer (Vertico-Class)**

**Goal**: Vertical, in-place completion

### **Capabilities**

* Minibuffer can grow vertically  
* Completion rendered *inside* minibuffer  
* Configurable height  
* Scrollable candidates

### **Required Additions**

* `ui/minibuffer-height-policy`  
* `minibuffer/render-list`  
* `minibuffer/scroll`

### **Success Criteria**

* Vertico-like UX possible  
* No layout corruption

---

## **v3 — Preview Without Commitment (Consult-Class)**

**Goal**: Speculative interaction

### **Capabilities**

* Cursor movement previews  
* Live buffer previews  
* Zero commit on abort

### **Required Additions**

* `preview/start`  
* `preview/update`  
* `preview/cancel`  
* `preview/commit`

### **Success Criteria**

* Consult-style commands feasible  
* Undo stack remains clean

---

## **v4 — Recursive Minibuffers (Power Feature)**

**Goal**: Embark-class composability

### **Capabilities**

* Minibuffer inside minibuffer  
* Stack-based restoration  
* Nested prompts

### **Required Additions**

* `minibuffer/stack`  
* `minibuffer/push`  
* `minibuffer/pop`

### **Success Criteria**

* Recursive `M-x` works  
* Embark-style actions possible

---

## **v5 — Actions, Metadata & Context (Embark-Class)**

**Goal**: Context-aware actions

### **Capabilities**

* Action dispatch on current candidate  
* Context-sensitive menus  
* Multiple action targets

### **Required Additions**

* `context/current`  
* `action/registry`  
* `action/dispatch`

### **Success Criteria**

* Embark-like workflows achievable

---

## **v6 — Theming & Dynamic Modes**

**Goal**: Make it feel native

### **Capabilities**

* Theme-aware minibuffer  
* Dynamic modeline changes  
* Per-state styling

### **Required Additions**

* `style/minibuffer-faces`  
* `modeline/context`

---

## **vN — Package Ecosystem Ready**

**Goal**: External packages can extend minibuffer safely

### **Capabilities**

* Public APIs  
* Stable contracts  
* SCI-backed extensions  
* Partial elisp compatibility (optional)

---

## **6\. What We Are Explicitly Not Doing (Yet)**

* No widget frameworks  
* No React-style components  
* No direct state mutation  
* No monolithic minibuffer UI

---

## **7\. Key Lessons from Emacs (Do Not Forget)**

1. The minibuffer is not special — **its contracts are**  
2. Power comes from **composability**  
3. Restoration is more important than expansion  
4. Preview must never corrupt state  
5. Themes and modes are behavior, not paint

---

## **8\. Final Recommendation**

Do **not** jump to v2 or v3.

If Lexicon nails:

* v0 correctness  
* v1 completion contract  
* v2 layout negotiation

…everything else becomes *much* easier.

---

## **Optional Next Steps**

If you want, I can:

* extract **exact Emacs minibuffer invariants**  
* design the **preview contract formally**  
* sketch a **Lexicon minibuffer API**  
* or map this directly to your current namespaces

This plan is ambitious — but it is also realistic, disciplined, and battle-tested by Emacs’ 40 years of evolution.

You are asking exactly the right questions.


Perfect — then we’ll **lock this down properly**, the way Emacs itself effectively did over decades, but *explicitly* and *intentionally*.

Below is a **complete package** consisting of:

1. **Formal minibuffer invariants (non-negotiable rules)**  
2. **A concrete Lexicon minibuffer API (data \+ commands)**  
3. **Preview & unwind contract (this is critical)**  
4. **Namespace & code-organization mapping**  
5. **How this plugs into theming, modeline, frames**  
6. **A disciplined execution plan (what to do first, second, third)**

This is not hand-wavy. You should be able to implement directly from this.

---

# **Lexicon Minibuffer — Full Design & Execution Specification**

---

## **1\. Minibuffer Invariants (Hard Rules)**

These are **laws**, not guidelines. Every phase (v0 → vN) must respect them.

### **I1 — The Minibuffer Is a Buffer**

* It has text  
* It has point  
* It has buffer-local state  
* It can run modes  
* It participates in undo (locally)

If the minibuffer is ever implemented as a widget, Lexicon has already lost.

---

### **I2 — All Minibuffer Effects Are Reversible**

* Abort (`C-g`) must leave **zero residue**  
* Layout, focus, previews, state must restore  
* Restoration must be *structural*, not heuristic

This implies **layout snapshots**, not “best effort”.

---

### **I3 — No Direct State Mutation**

* Minibuffer never mutates editor state directly  
* All changes go through commands  
* Preview changes must be fenced

This preserves Lexicon’s command-based core.

---

### **I4 — Recursive by Design**

* Minibuffer must support nesting  
* Each invocation is isolated  
* Exit unwinds exactly one level

No “global minibuffer singleton”.

---

### **I5 — Expansion Is Negotiated, Not Hardcoded**

* Minibuffer may request space  
* Window system decides  
* On exit, space is returned

This prevents UI corruption and flicker.

---

## **2\. Core Data Model (This Is the Spine)**

### **2.1 Minibuffer Session**

{:id                uuid  
 :prompt            string  
 :buffer-id         buffer-id  
 :frame-id          frame-id  
 :parent-session    session-id | nil  
 :completion        completion-state | nil  
 :preview           preview-state | nil  
 :layout-snapshot   layout-snapshot  
 :on-accept         command  
 :on-abort          command  
 :metadata          map}

This object is **the unit of control**.

---

### **2.2 Minibuffer Stack**

{:stack \[session-1 session-2 ... session-n\]}

* Push on entry  
* Pop on exit  
* Never mutate previous sessions

---

## **3\. Lexicon Minibuffer API (Concrete, Minimal)**

This is the **public internal API**. Keep it small.

### **3.1 Entry / Exit**

(minibuffer/start  
  {:prompt "M-x "  
   :on-accept command  
   :on-abort command  
   :completion completion-spec  
   :preview preview-spec})

(minibuffer/accept)  
(minibuffer/abort)

---

### **3.2 Reading Input**

(minibuffer/text)        ;; current input  
(minibuffer/point)  
(minibuffer/set-text\!)

(Internally implemented via normal buffer ops.)

---

### **3.3 Completion**

{:completion/type :none | :list | :dynamic  
 :completion/fn   (fn \[input\] candidates)  
 :completion/render :inline | :separate-window}

The minibuffer **does not** implement completion logic — it orchestrates it.

---

### **3.4 Preview Contract (Very Important)**

{:preview/start   (fn \[\])  
 :preview/update  (fn \[candidate\])  
 :preview/cancel  (fn \[\])  
 :preview/commit  (fn \[candidate\])}

Rules:

* `start` called once  
* `update` called many times  
* `cancel` OR `commit` must be called exactly once  
* `cancel` must restore everything

This mirrors `unwind-protect` semantics in Emacs.

---

## **4\. Layout & Window Contract**

### **4.1 Layout Snapshot**

Before minibuffer entry:

(ui/capture-layout frame-id)

On exit:

(ui/restore-layout snapshot)

No exceptions. Ever.

---

### **4.2 Expansion Policy**

{:min-lines 1  
 :max-lines 10  
 :resize-policy :grow-only | :elastic}

Window system decides *how* to satisfy this request.

---

## **5\. Modeline & Theming Integration**

### **5.1 Minibuffer Context**

Expose a **context map**:

{:minibuffer/active? true  
 :minibuffer/depth 2  
 :minibuffer/type :command | :file | :buffer  
 :minibuffer/completion :vertical}

Modeline can react to this *without knowing minibuffer internals*.

---

### **5.2 Styling (Faces, Not Widgets)**

Use semantic roles:

* `:minibuffer/prompt`  
* `:minibuffer/input`  
* `:minibuffer/candidate`  
* `:minibuffer/active-candidate`

Themes style roles, not components.

---

## **6\. Namespace Mapping (Very Important for Hygiene)**

This keeps the implementation **comprehensible** long-term.

lexicon.minibuffer.core        ;; session lifecycle  
lexicon.minibuffer.stack       ;; recursion  
lexicon.minibuffer.input       ;; buffer ops  
lexicon.minibuffer.completion  ;; orchestration  
lexicon.minibuffer.preview     ;; preview contract  
lexicon.minibuffer.ui          ;; rendering & layout  
lexicon.minibuffer.theme       ;; faces & styling

Do **not** merge these prematurely.

---

## **7\. Versioned Execution Plan (Concrete Work Units)**

### **v0 — Session & Abort Correctness**

* `minibuffer/start`  
* layout snapshot \+ restore  
* accept / abort  
* single-line buffer

📌 **Deliverable**: `M-x` works, `C-g` is perfect

---

### **v1 — Completion via Separate Window**

* `completion/fn`  
* TAB triggers  
* candidates rendered elsewhere

📌 **Deliverable**: `find-file`, `switch-buffer`

---

### **v2 — Vertical Expansion**

* inline candidate rendering  
* dynamic height negotiation

📌 **Deliverable**: Vertico-class UX possible

---

### **v3 — Preview Semantics**

* fenced preview lifecycle  
* no undo pollution

📌 **Deliverable**: Consult-class commands

---

### **v4 — Recursive Minibuffers**

* stack discipline  
* nested prompts

📌 **Deliverable**: Embark-class actions

---

### **v5 — Actions & Metadata**

* contextual actions  
* candidate metadata

📌 **Deliverable**: Ecosystem-ready

---

## **8\. Why This Will Work (Reality Check)**

This plan works because:

* It copies **Emacs’ contracts**, not its code  
* It aligns with Lexicon’s **command-only mutation**  
* It scales from trivial to powerful  
* It allows partial success at every phase  
* It avoids UI dead ends

Most editors fail here because they skip v0 and v1.  
You are explicitly *not* doing that.

---

## **9\. Final Advice (Important)**

Treat the minibuffer like:

* a **language runtime**  
* not a UI component

If you implement the **contracts first**, the UX will follow naturally.

---

If you want next, I can:

* write the **v0 implementation checklist**  
* sketch tests for abort/restore correctness  
* map Emacs minibuffer Lisp APIs → Lexicon equivalents  
* or help you decide *where* to start coding in your repo

This is one of the hardest problems in editor design — and you are now approaching it the *right* way.


# Minibuffer Research Archive

**Date:** 2026-01-04

This directory contains the research documents that led to the final **MinibufferArchitecture.md** design.

## Documents

1. **EmacsMiniBufDeepDive.md** - Initial deep conceptual research
   - Identified minibuffer as "control plane" not widget
   - Documented avatars (usage modes)
   - Expansion/contraction mechanics
   - Why Vertico/Consult feel magical

2. **EmacsMiniBufPlan1.md** - First design specification
   - Design goals and invariants
   - Primitive mapping (Emacs → Lexicon)
   - Versioned roadmap (v0 → vN)
   - Formal contracts

3. **EmacsMiniBufPlan2.md** - Full execution specification
   - Minibuffer invariants (hard rules)
   - Core data model
   - API design (entry/exit/completion/preview)
   - Namespace mapping
   - Implementation plan

4. **MinibufferDesignRefinement.md** - Critical analysis based on Emacs docs
   - Validation of initial research
   - Terminology corrections ("always visible" → "always-allocated window")
   - Preview pattern refinement
   - Missing features identified (hooks, history, keymaps)

## Outcome

All research consolidated into:
- **`docs/MinibufferArchitecture.md`** - Single authoritative design document
- **`docs/ROADMAP.md` Phase 7.8.1** - Implementation plan

## Key Insights Carried Forward

✅ **Validated:**
- Minibuffer as control plane (not UI widget)
- Recursive by design (stackable contexts)
- Dynamic expansion (can steal frame space)
- Completion is external (pluggable)
- Preview without commit (speculative interaction)

⚠️ **Refined:**
- "Always visible" → "Always-allocated window" (window exists, buffer changes)
- Preview: Four functions → Single state-fn pattern
- Added hooks, history, minibuffer modes
- Echo area is same window as minibuffer (not separate)

## Research Methodology

1. Conceptual analysis (DeepDive)
2. Design specification (Plan1, Plan2)
3. Web research (Emacs docs, Vertico/Consult)
4. **Source code analysis** (`src/minibuf.c`, `lisp/minibuffer.el`)
5. Refinement and consolidation

## Status

**ARCHIVED** - Research complete, implementation in progress per Phase 7.8.1

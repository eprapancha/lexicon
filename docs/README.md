# Lexicon Documentation

**Quick Start:** Read files in this order:
1. `.CLAUDE.md` (project root) - AI context & current status
2. `ROADMAP.md` - Phase tracking & progress
3. `SESSION_HANDOFF.md` - Latest session summary (if picking up work)

---

## Core Documentation

**Essential Reading:**
- **[ROADMAP.md](./ROADMAP.md)** - Phase-by-phase development plan (THE source of truth for progress)
- **[CORE_PRINCIPLES.md](./CORE_PRINCIPLES.md)** - Architectural philosophy ("Do what Emacs does")
- **[SESSION_HANDOFF.md](./SESSION_HANDOFF.md)** - Latest session summary (updated after each work session)

**Architecture:**
- **[architecture.md](./architecture.md)** - Technical architecture details
- **[MinibufferArchitecture.md](./MinibufferArchitecture.md)** - Minibuffer redesign plan (Phase 7.8.1)
- **[EmacsAPICompatibilityAudit.md](./EmacsAPICompatibilityAudit.md)** - API gap analysis (what's missing vs Emacs)
- **[EmacsCompatibilityImplementation.md](./EmacsCompatibilityImplementation.md)** - Implementation record (what was added)

**Development:**
- **[development-setup.md](./development-setup.md)** - Setup instructions
- **[ManualTestingPlan.md](./ManualTestingPlan.md)** - Manual testing checklist
- **[ELISP_COMPATIBILITY.md](./ELISP_COMPATIBILITY.md)** - Elisp compatibility strategy (Phase 7.9+)

**Reference:**
- **[EmacsReferenceCard.md](./EmacsReferenceCard.md)** - Quick Emacs command reference

---

## Documentation Organization

### Active Documents (Keep Updated)
- Core status: `ROADMAP.md`, `SESSION_HANDOFF.md`
- Architecture: `CORE_PRINCIPLES.md`, `architecture.md`, `MinibufferArchitecture.md`
- API tracking: `EmacsAPICompatibilityAudit.md`, `EmacsCompatibilityImplementation.md`
- Development: `ManualTestingPlan.md`, `development-setup.md`

### Reference (Read-Only)
- `EmacsReferenceCard.md` - Emacs command reference
- `ELISP_COMPATIBILITY.md` - Future elisp strategy

### Archived
- `archive/session-specific/` - Session-specific work (historical reference only)
- `archive/minibuffer-research/` - Research documents that informed MinibufferArchitecture.md

---

## When to Update Each Document

**After Every Session:**
- Update `SESSION_HANDOFF.md` with what was completed, test status, next steps
- Update `.CLAUDE.md` (project root) with completion status

**After Completing a Phase:**
- Mark phase complete in `ROADMAP.md`
- Update `EmacsCompatibilityImplementation.md` if APIs were added
- Archive any phase-specific planning docs

**When Discovering Architecture Issues:**
- Document in `architecture.md` or create focused doc (like `MinibufferArchitecture.md`)
- Link from `ROADMAP.md` and `.CLAUDE.md`

**When APIs Are Added:**
- Update `EmacsAPICompatibilityAudit.md` to mark APIs as implemented
- Document in `EmacsCompatibilityImplementation.md`

---

## Documentation Principles

1. **Single Source of Truth** - Don't duplicate information across files
2. **Link, Don't Copy** - Reference other docs instead of copying content
3. **Active vs. Archive** - Move completed/obsolete docs to `archive/`
4. **Session Handoffs** - Always update `SESSION_HANDOFF.md` before taking a break
5. **Git as History** - Use git for historical context, keep docs focused on current state

---

## Questions?

- **What's the current status?** → Read `SESSION_HANDOFF.md`
- **What's the big picture plan?** → Read `ROADMAP.md`
- **Why was this decision made?** → Read `CORE_PRINCIPLES.md`
- **What APIs are missing?** → Read `EmacsAPICompatibilityAudit.md`
- **How do I test?** → Read `ManualTestingPlan.md`
- **How do I set up dev environment?** → Read `development-setup.md`

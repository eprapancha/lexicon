---
name: reconcile
description: Check if project documentation is up-to-date with recent code changes. Flags stale docs that need updating after new features, API changes, or package additions.
argument-hint: [scope e.g. "full", "recent", "packages", "api"]
allowed-tools: Read, Glob, Grep, Bash(git diff*, git log*)
---

# Documentation Reconciliation

Check whether project documentation accurately reflects the current state of the codebase. Produce a reconciliation report that flags stale, missing, or inaccurate documentation.

---

## Process

1. **Determine scope** from the argument:
   - `full` -- check everything against current codebase state
   - `recent` -- check only against recent git changes (last N commits or since last tag)
   - `packages` -- focus on package-related docs (Available Packages table, lexpkgs, lexpa)
   - `api` -- focus on API surface (lisp.cljs sci-namespace count, function lists)
2. **Gather current state** from the codebase
3. **Compare against documentation**
4. **Output a reconciliation report**

---

## Documents to Check

| Document | What to verify |
|----------|---------------|
| `README.org` | Available Packages table, feature list, architecture diagram, project structure |
| `CLAUDE.md` | Important Paths, key commands, state ownership map |
| `docs/ROADMAP.org` | "What's Been Built" section, "Active Work" section |
| `docs/VISION.org` | Sequencing section, current status references |
| `.claude/agents/SHARED_CONTEXT.md` | SCI infrastructure table, trust levels, lexpkgs list, API count |
| `.claude/agents/package-builder.md` | Available API categories, package structure example |
| `.claude/skills/chrome-test/SKILL.md` | Test banks for new features |

---

## Checks to Perform

### 1. Package Count

```bash
# Count packages in lexpkgs
ls -d ~/projects/lexpkgs/*/package.edn 2>/dev/null | wc -l

# Count recipes in lexpa
ls ~/projects/lexpa/recipes/*.edn 2>/dev/null | wc -l
```

Compare against:
- `README.org` Available Packages table (row count)
- `ROADMAP.org` Completed Packages list
- `SHARED_CONTEXT.md` lexpkgs list

### 2. API Function Count

```bash
# Count sci-namespace entries
grep -c "^  '" packages/editor-cljs/src/lexicon/lisp.cljs
```

Compare against:
- `README.org` "230+ API functions" claim
- `SHARED_CONTEXT.md` "~230+ functions" claim
- `package-builder.md` "~200+ functions" claim

### 3. Feature Inventory

Check for features implemented in code but not mentioned in docs:
- New commands registered in `command.cljs`
- New modes defined in `modes/`
- New completion styles in `styles.cljs`
- New API functions in `lisp.cljs`

### 4. Recent Changes (for `recent` scope)

```bash
# Files changed in last 5 commits
git log --oneline -5 --name-only

# What changed in lisp.cljs (API surface)
git diff HEAD~5 -- packages/editor-cljs/src/lexicon/lisp.cljs | head -100

# New packages added
git diff HEAD~5 -- lexpkgs/
```

### 5. Structural Accuracy

- `README.org` project structure matches actual directory layout
- Architecture diagram reflects current component relationships
- Build commands in docs match `bb.edn` tasks

---

## Output Format

```
## Documentation Reconciliation Report

### Scope: [full/recent/packages/api]
### Date: [today]

### Summary
- Documents checked: N
- Issues found: N (X critical, Y minor)

### Critical Issues (docs are wrong)
1. **[document] -- [section]**
   - Current doc says: [quote]
   - Reality: [what's actually true]
   - Fix: [specific edit needed]

### Minor Issues (docs are incomplete)
1. **[document] -- [section]**
   - Missing: [what should be added]

### Up-to-Date (no issues)
- [document] -- [section]: accurate
```

---

## Rules

- NEVER modify documentation files yourself -- only report what needs changing
- Be specific: quote the stale text and state what it should say
- Distinguish between "wrong" (critical) and "incomplete" (minor)
- If a count is approximate (e.g., "230+"), flag it only if the real count has diverged significantly (>10%)
- Check git blame to see when docs were last updated vs when code changed

---

## When to Use This Skill

Run `/reconcile` after:
- Adding a new package to lexpkgs
- Adding new API functions to lisp.cljs
- Completing a major feature
- Before creating a release or updating the README
- Periodically (e.g., weekly) to catch drift

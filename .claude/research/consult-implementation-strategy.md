# Consult + Embark Implementation Strategy

**Date:** 2026-05-03
**Depends on:** consult-study.md, embark-study.md, wasm-ecosystem.md

---

## Goal

Implement consult and embark in Lexicon with zero compromises. Where Emacs shells out to external tools, Lexicon uses WASM equivalents running in-browser. The implementation should produce modular subprojects that have standalone value.

---

## Architecture: Five Layers

Each layer is independently valuable and testable. Each can be a standalone package. If Lexicon fails, these survive.

### Layer 1: Core Completion Infrastructure

**What:** The framework that ALL consult commands need. Pure ClojureScript, no WASM.

**Components:**
- **Timer system** -- `run-with-timer`, `run-with-idle-timer`, `cancel-timer`. Consult's async pipeline depends heavily on timers for debouncing (0.2s), throttling (0.5s), and refresh delays (0.2s).
- **Completion metadata enrichment** -- `group-function`, `affixation-function`, `display-sort-function` in completing-read metadata. Currently stub/missing.
- **Preview framework** -- State function protocol (setup/preview/exit/return lifecycle). Jump preview (move point, highlight line), buffer preview (switch buffer in preview window), insertion preview (overlay showing candidate text).
- **Multi-source framework** -- `consult--multi` equivalent: multiple candidate sources with narrowing keys, per-source categories, tofu encoding for source identification, hidden sources.
- **Async pipeline** -- The functional pipeline: split -> options -> min-input -> throttle -> process/dynamic -> transform -> highlight -> indicator -> refresh -> sink. Message protocol (setup/destroy/flush/refresh/cancel).
- **Candidate collection API** -- Access to ALL current completion candidates (not just selected). Needed by both consult and embark.

**Lexicon gaps to fill:**
- Timer system (currently missing or minimal)
- Completion metadata (group-function, affixation-function not wired through)
- Preview infrastructure (no state function protocol)
- Async completion (completing-read is synchronous today)

**API additions needed in lisp.cljs:**
- `run-with-timer`, `run-with-idle-timer`, `cancel-timer`
- `completion-metadata-get` (read metadata from completion tables)
- `set-completion-metadata` (set metadata on completion tables)
- `minibuffer-selected-window` (for preview target window)
- `window-configuration-to-register`, `set-window-configuration` (for preview save/restore)

### Layer 2: Buffer-Local Consult Commands

**What:** Consult commands that only search open buffers and internal state. No filesystem, no external tools.

**Commands (priority order):**

1. `consult-line` -- Search for matching line in current buffer. The flagship command.
   - Needs: buffer text scanning, line-number-at-pos, markers, overlay preview
   - Infrastructure: jump preview, match highlighting

2. `consult-buffer` -- Enhanced switch-to-buffer with multiple sources.
   - Needs: multi-source framework, buffer-list, recentf, bookmarks
   - Sources: buffers, hidden buffers, modified buffers, recent files, bookmarks

3. `consult-outline` -- Jump to outline heading.
   - Needs: outline-regexp, outline-level, heading scanning

4. `consult-goto-line` -- Jump to line number with preview.
   - Needs: jump preview, display-line-numbers integration

5. `consult-mark` / `consult-global-mark` -- Jump to mark ring entries.
   - Needs: mark-ring, global-mark-ring, marker system

6. `consult-yank-from-kill-ring` -- Select from kill ring with insertion preview.
   - Needs: kill-ring access, insertion preview overlay

7. `consult-history` -- Insert from mode/minibuffer history.
   - Needs: history variables, comint-input-ring (if shell exists)

8. `consult-imenu` -- Flat imenu with preview and narrowing.
   - Needs: imenu-create-index-function, imenu infrastructure

9. `consult-bookmark` -- Select or create bookmark.
   - Needs: bookmark-alist, bookmark-jump, bookmark-set

10. `consult-register` -- Browse and use registers with preview.
    - Needs: register system (currently missing/stub)

11. `consult-focus-lines` / `consult-keep-lines` -- Filter buffer lines in-place.
    - Needs: overlay system (have it), regex matching

12. `consult-theme` -- Select and preview themes.
    - Needs: theme system (currently stub)

13. `consult-minor-mode-menu` -- Toggle minor modes.
    - Needs: minor-mode-list (have it)

14. `consult-isearch-history` -- Browse isearch history.
    - Needs: search-ring, regexp-search-ring

15. `consult-line-multi` -- Search across multiple buffers.
    - Needs: dynamic async computation, buffer traversal

**What this layer exercises:**
- The full completion pipeline with metadata
- Preview (jump, buffer, insertion)
- Multi-source with narrowing
- Grouping and annotation
- History integration

### Layer 3: Virtual Filesystem (`@lexicon/vfs`)

**What:** Browser-native filesystem abstraction. Standalone npm package.

**Architecture:**
```
@lexicon/vfs
  |
  +-- backends/
  |     +-- opfs.ts       -- OPFS (primary, fast, sync in Workers)
  |     +-- idb.ts        -- IndexedDB (fallback, universal)
  |     +-- memory.ts     -- In-memory (scratch/temp)
  |     +-- fsa.ts        -- File System Access API (Chromium local files)
  |
  +-- vfs.ts              -- Unified API (Node.js fs-compatible)
  +-- watcher.ts          -- File change notifications
  +-- glob.ts             -- Glob pattern matching
  +-- gitignore.ts        -- .gitignore-style exclusion
```

**Key design decisions:**
- OPFS as primary backend (2x faster than IndexedDB, sync in Workers, no permissions)
- IndexedDB fallback for browsers without OPFS
- File System Access API for opening real local directories (Chromium only)
- In-memory for temp/scratch buffers
- Node.js `fs`-compatible API surface for WASM tool compatibility
- File change watchers for auto-revert
- Multi-tab synchronization via BroadcastChannel

**What this enables:**
- `consult-find` / `consult-fd` -- pure JS file discovery against VFS
- Real `dired` with persistent directories
- Project detection (look for .git, package.json, etc.)
- File-backed buffers that persist across sessions

**Standalone value:** Every browser-based IDE needs this. LightningFS is minimal, BrowserFS is abandoned. A modern OPFS-first implementation would be widely adopted.

### Layer 4: WASM Grep (`@lexicon/wasm-grep`)

**What:** ripgrep's search engine compiled to WASM. Standalone npm package.

**Architecture:**
```
@lexicon/wasm-grep
  |
  +-- rust/
  |     +-- lib.rs        -- WASM interface (wasm-bindgen)
  |     +-- Cargo.toml    -- depends on grep-searcher, grep-regex, grep-matcher
  |
  +-- js/
  |     +-- index.ts      -- JS API: search(pattern, bytes, options) -> matches[]
  |     +-- streaming.ts  -- Streaming search for large files
  |     +-- worker.ts     -- Web Worker wrapper for non-blocking search
  |
  +-- wasm/
        +-- grep_bg.wasm  -- Compiled WASM (~700KB estimated)
```

**Interface:**
```typescript
interface GrepMatch {
  line_number: number;
  byte_offset: number;
  line_text: string;
  match_start: number;
  match_end: number;
}

// Core search function
search(pattern: string, content: Uint8Array, options?: {
  case_sensitive?: boolean;
  regex_type?: 'pcre' | 'extended' | 'literal';
  max_count?: number;
  context_lines?: number;
}): GrepMatch[];

// Streaming search for large files
searchStream(pattern: string, stream: ReadableStream<Uint8Array>): AsyncIterable<GrepMatch>;
```

**Integration with Lexicon:**
- JS/CLJS enumerates files from VFS
- Reads file contents as byte arrays
- Passes to WASM grep for matching
- Returns structured results
- Async pipeline wraps this in consult's debounce/throttle/refresh

**What this enables:**
- `consult-ripgrep` -- full async grep with streaming results
- `consult-grep` -- same engine, different defaults
- `occur` improvements -- WASM-speed regex matching
- `query-replace-regexp` -- faster regex on large buffers

**Standalone value:** Very high. Any browser-based editor, documentation site, or code review tool could use fast regex search. Netgrep proves the concept; a polished package fills a real gap.

### Layer 5: Process Model & Shell (`@lexicon/wasm-kernel`)

**What:** Unix-like process semantics in the browser. The most ambitious layer.

**Architecture (based on Browsix):**
```
@lexicon/wasm-kernel
  |
  +-- kernel/
  |     +-- kernel.ts         -- Main thread kernel
  |     +-- syscalls.ts       -- Syscall implementations
  |     +-- process-table.ts  -- Process tracking
  |     +-- pipe.ts           -- Inter-process pipes
  |     +-- signals.ts        -- Signal delivery
  |
  +-- worker/
  |     +-- process.ts        -- Worker-side process wrapper
  |     +-- wasi-shim.ts      -- WASI syscall translation
  |
  +-- shell/
        +-- shell.wasm        -- WASIX-compiled Bash (from Wasmer)
        +-- coreutils.wasm    -- Basic utilities (ls, cat, grep, etc.)
```

**Key mechanism:** SharedArrayBuffer + Atomics
- Process (in Worker) calls syscall, writes args to SharedArrayBuffer, calls `Atomics.wait()`
- Kernel (main thread) reads syscall, processes it, writes result, calls `Atomics.notify()`
- This enables synchronous blocking I/O from the process's perspective
- Requires cross-origin isolation headers (`COOP: same-origin`, `COEP: require-corp`)

**What this enables:**
- `consult-man` -- run `man -k` as a real process
- Real `compile` / `recompile` -- run build commands
- `shell` / `eshell` with real process spawning
- `consult-git-grep` -- run real git
- Eventually: LSP servers, language tools

**Standalone value:** High but niche. A modern Browsix replacement built with current APIs would serve browser IDEs and educational platforms.

---

## Embark Integration

Embark sits orthogonally to the layers above. It needs:

**From Layer 1:**
- Target finder framework (hook chain for thing-at-point)
- Per-type action keymaps with inheritance
- Target injection into minibuffer (recursive minibuffers -- requires minibuffer stack, currently flat map)
- Candidate collection API
- Indicator system (overlay-based)

**From Layer 2:**
- Export infrastructure (consult-location -> occur-mode, consult-grep -> grep-mode)
- embark-consult bridge (search map, candidate collectors, preview in collect buffers)

**Critical Lexicon gap:** Recursive minibuffers. The minibuffer is currently a flat `:minibuffer` map. Embark's injection trick opens a new minibuffer while one is already active. This requires refactoring to a minibuffer stack. This is a foundational change that should happen in Layer 1.

---

## Implementation Order

```
Layer 1: Core completion infrastructure
  |
  +---> Layer 2: Buffer-local consult commands
  |       |
  |       +---> Embark (target/action framework)
  |               |
  |               +---> embark-consult bridge
  |
  +---> Layer 3: Virtual filesystem (@lexicon/vfs)
  |       |
  |       +---> consult-find, consult-fd (pure JS against VFS)
  |       |
  |       +---> Layer 4: WASM grep (@lexicon/wasm-grep)
  |               |
  |               +---> consult-ripgrep, consult-grep
  |
  +---> Layer 5: Process model (@lexicon/wasm-kernel)
          |
          +---> consult-man, compile, shell
          +---> consult-git-grep
```

Layers 2, 3, and 5 can progress in parallel once Layer 1 is solid.

---

## Subproject Extraction Timeline

| Phase | Subproject | Repo | Standalone Value |
|-------|-----------|------|-----------------|
| Early | `@lexicon/vfs` | `lexicon-vfs` | Very high |
| Early | `@lexicon/tree-sitter-bridge` | `lexicon-tree-sitter` | High |
| Mid | `@lexicon/wasm-grep` | `lexicon-wasm-grep` | Very high |
| Late | `@lexicon/wasm-kernel` | `lexicon-wasm-kernel` | High |
| Late | `@lexicon/wasm-shell` | `lexicon-wasm-shell` | High |

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|-----------|
| ripgrep WASM binary too large (700KB+) | Medium | Medium | Strip Unicode tables, lazy-load, tree-shake |
| SharedArrayBuffer requires COOP/COEP headers | Certain | Medium | Document deployment requirements; degrade gracefully |
| Minibuffer stack refactor breaks existing features | Medium | High | Extensive E2E tests before/after; incremental migration |
| OPFS not available in all browsers | Low | Low | IndexedDB fallback; OPFS has broad support |
| Timer precision in browsers (requestAnimationFrame throttling) | Medium | Low | Use performance.now(), requestIdleCallback for non-critical |
| Async completing-read breaks existing synchronous callers | High | High | Dual-mode: sync for simple, async for consult; progressive enhancement |

---

## Open Questions

1. Should the VFS be a separate npm package from day one, or extracted later?
2. Should the WASM grep module live in `packages/lexicon-engine/` alongside the gap buffer, or be a separate repo?
3. How to handle the minibuffer stack refactor without breaking Vertico/Orderless/Marginalia?
4. Should we adopt Emscripten or wasm-bindgen for the grep WASM module? (Current gap buffer uses wasm-bindgen)
5. What's the minimum viable consult that's useful enough to ship?

---

## References

- `.claude/research/consult-study.md` -- Full consult package analysis
- `.claude/research/embark-study.md` -- Full embark package analysis
- `.claude/research/wasm-ecosystem.md` -- WASM feasibility research
- `docs/VISION.org` -- Project vision (WASM core expansion section)
- `docs/ROADMAP.org` -- Development roadmap

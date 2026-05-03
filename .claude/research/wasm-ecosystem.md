# WASM Ecosystem Research for Lexicon

**Date:** 2026-05-03

---

Now I have enough information to compile a comprehensive research report. Here it is:

---

# Research Report: Unix/GNU Tools in WebAssembly for Browser Use

## 1. ripgrep in WASM

### Current Status

Compiling ripgrep (or its underlying crates) to WASM is proven and has been done by multiple projects:

**Netgrep** ([github.com/dgopsq/netgrep](https://github.com/dgopsq/netgrep)) by Diego Pasquali is the most relevant browser-targeted effort. It forks ripgrep to fix WASM compatibility issues and exposes only the core search functionality -- specifically the `search_slice` method from the `grep` crate. The architecture splits ripgrep's work into two phases: **Discovery** (listing files) and **Search** (matching patterns in byte slices). Netgrep implements only Search, leaving file discovery to the caller. The `search_bytes` function analyzes a slice of bytes and returns whether a pattern was found, allowing streaming search as data is downloaded. ([Blog post](https://diegopasquali.com/blog/ripgrep-powered-search-engine-on-the-web))

**The `ripgrep` npm package** (v0.3.1, published April 2025) cross-compiles ripgrep to `wasm32-wasip1` with SIMD enabled via `cargo zigbuild`. The resulting `.wasm` is brotli-compressed and z85-encoded into an ESM module. It uses a minimal WASI preview1 shim (~20 syscalls) and works on Node.js, Bun, and Deno. The compiled `WebAssembly.Module` is memoized in-process. ([npm: ripgrep](https://www.npmjs.com/package/ripgrep))

**`ripgrep_wasm` Ruby gem** compiles ripgrep 15.1.0 to WASM for WASI-compatible runtimes like Wasmtime. ([Libraries.io](https://libraries.io/rubygems/ripgrep_wasm))

### Blockers for Full Browser Use

1. **Filesystem access**: The full ripgrep CLI traverses directories using OS-level filesystem APIs. Browsers provide no equivalent. This is the fundamental blocker for running ripgrep as-is in a browser.
2. **mmap**: ripgrep uses memory-mapped files for performance. WASM has no mmap equivalent.
3. **Threading**: ripgrep uses parallelized directory walking and searching. Browser WASM threading requires SharedArrayBuffer and cross-origin isolation headers, which adds deployment complexity.
4. **Binary size**: The Rust `regex` crate with default features adds approximately 500KB to WASM binaries due to Unicode table data. A minimal function using regex compiled to ~720KB; removing regex brought it down to ~24KB. ([esimmler.com](https://esimmler.com/large-wasm-builds-with-rust-regex))

### BurntSushi's Assessment

In a [GitHub discussion](https://github.com/BurntSushi/ripgrep/discussions/2067), BurntSushi (ripgrep's author) acknowledged that using ripgrep's crates as a library and compiling to WASM is "technically possible," but said "feasible" is not the word he would use unless you have significant resources. He suggested either porting to JavaScript or creating a C API for ripgrep's crates.

### Alternative Approaches for Lexicon

The most practical path is to use ripgrep's crate ecosystem (`grep-searcher`, `grep-regex`, `grep-matcher`) directly as a Rust library compiled to WASM, bypassing the CLI entirely. The interface would be:
- Pass in byte slices (file contents already in WASM memory)
- Pass in a pattern (regex)
- Get back match locations (line numbers, byte offsets, matched text)

This is essentially what Netgrep does. The filesystem traversal would be handled in JavaScript/ClojureScript, with only the actual regex searching delegated to WASM.

**ast-grep** ([ast-grep.github.io](https://ast-grep.github.io/)) is also worth noting -- it provides `@ast-grep/wasm` for AST-based code search/rewrite using tree-sitter, compiled to WASM. This could complement regex-based search for structural code queries.

### Feasibility Rating: **Feasible with effort**

The core search engine works in WASM today. The integration work involves building a JavaScript-side file discovery layer that feeds byte slices to the WASM search engine.

---

## 2. GNU find / fd in WASM

### Current Status

There is **no known effort** to compile `fd` ([github.com/sharkdp/fd](https://github.com/sharkdp/fd)) to WASM for browser use. This makes sense given:
- `fd` is fundamentally a filesystem traversal tool that depends on OS-level directory walking APIs
- It uses parallelized directory traversal via the `ignore` crate (same crate ripgrep uses)
- Browser sandboxing prevents direct filesystem access

### What Would Be Needed

For Lexicon, "find" functionality means traversing a virtual filesystem. Since the filesystem is entirely in-memory or in IndexedDB, the traversal logic can be implemented directly in ClojureScript/JavaScript without WASM. The key operations are:
- Recursive directory listing with glob pattern matching
- Filtering by filename, extension, file type
- Respecting `.gitignore`-style exclusion patterns

The `ignore` crate (used by both ripgrep and fd) handles gitignore parsing and glob matching. Its core pattern matching could theoretically be compiled to WASM, but the filesystem traversal would need to be reimplemented against whatever virtual filesystem API Lexicon uses.

### Feasibility Rating: **Ready now (reimplementation)**

File finding against a virtual filesystem is straightforward to implement in JavaScript. There is no benefit to compiling fd to WASM -- the filesystem is already JavaScript-accessible. For gitignore-style filtering, a JavaScript library like `ignore` (npm) or `micromatch` would suffice.

---

## 3. WASM Virtual Filesystem

### Emscripten's Filesystem Layer

Emscripten provides a Unix-like virtual filesystem ([docs](https://emscripten.org/docs/api_reference/Filesystem-API.html)) with several backends:

| Backend | Description | Persistence | Thread-safe |
|---------|------------|-------------|-------------|
| **MEMFS** | In-memory, default | None (lost on reload) | No |
| **IDBFS** | IndexedDB-backed MEMFS | IndexedDB (best-effort) | No |
| **WORKERFS** | Read-only File/Blob access in Workers | N/A | Worker-only |
| **NODEFS** | Node.js fs passthrough | Native FS | Node-only |
| **PROXYFS** | Mounts another module's FS | Via proxied module | No |
| **WasmFS** | Next-gen, compiled to Wasm | OPFS-backed option | Yes (multithreaded) |

**Key finding**: [PROXYFS](https://emscripten.org/docs/api_reference/Filesystem-API.html) enables sharing a filesystem between multiple WASM module instances: `module2.FS.mount(module2.PROXYFS, { root: "/", fs: module1.FS }, "/fs1")`. This is relevant if Lexicon runs multiple WASM modules that need a shared filesystem view.

**WasmFS** ([Emscripten docs](https://emscripten.org/docs/porting/files/file_systems_overview.html)) is the next-generation replacement, compiled to Wasm with full multithreading support. It can use OPFS as a backend. However, a [GitHub issue from 2025](https://github.com/emscripten-core/emscripten/issues/24639) reports that WasmFS+OPFS is 2x slower than IDBFS for small reads/writes (<512 bytes), though it is faster for large operations.

### Origin Private File System (OPFS)

[OPFS](https://developer.mozilla.org/en-US/docs/Web/API/File_System_API/Origin_private_file_system) is a modern browser API that provides a sandboxed, high-performance filesystem per origin:

- **No permission prompts** -- unlike the File System Access API
- **Synchronous API** available in Web Workers via `createSyncAccessHandle()` -- critical for WASM programs expecting synchronous file I/O
- **Persistent** across browser restarts
- **2x faster** than IndexedDB for plain inserts ([web.dev](https://web.dev/articles/origin-private-file-system))
- **Browser support**: All major browsers (Chrome, Firefox, Safari) -- it is standardized by WHATWG
- **Quota limits**: Dynamically allocated, typically 300MB to several GB depending on device

The flagship use case is [SQLite compiled to WASM with OPFS backing](https://developer.chrome.com/blog/sqlite-wasm-in-the-browser-backed-by-the-origin-private-file-system) -- Google Chrome's team specifically designed OPFS to enable this.

### File System Access API

The [File System Access API](https://developer.chrome.com/docs/capabilities/web-apis/file-system-access) allows web apps to read and write to the user's real local filesystem:

- Used by vscode.dev to open local folders ([blog post](https://nasserspace.hashnode.dev/vscode-file-system-access-api))
- Requires user permission (file picker interaction)
- **Browser support**: Chrome, Edge, Opera, Brave (Chromium-based only). **Not supported** in Firefox or Safari.
- Provides `FileSystemHandle` objects that persist across sessions if stored in IndexedDB

### VS Code Web's Approach

[VS Code for the Web](https://code.visualstudio.com/docs/setup/vscode-web) uses a layered filesystem architecture ([Virtual Workspaces docs](https://code.visualstudio.com/api/extension-guides/virtual-workspaces)):
- **Virtual File System Providers**: Extensions register `FileSystemProvider` implementations for custom URI schemes (`vscode-vfs`, `github`, etc.)
- **`vscode.workspace.fs` API**: Unified abstraction layer that delegates to the appropriate provider
- **Remote repositories** (e.g., GitHub): Loaded via service APIs directly from the browser -- no cloning
- **Local files**: File System Access API (`showOpenFilePicker`)

### Standalone IndexedDB Filesystem Libraries

**LightningFS** ([github.com/isomorphic-git/lightning-fs](https://github.com/isomorphic-git/lightning-fs)) is a lean IndexedDB-backed `fs` implementation created for isomorphic-git:
- Implements a subset of the Node.js `fs` API plus `fs.promises`
- Uses IndexedDB as sole backend (after systematically ruling out localStorage, WebSQL, etc.)
- In-memory cache with 500ms debounce to IndexedDB
- Multi-threading support via mutex (atomic compare-and-replace in IndexedDB)
- Simple API: `const fs = new FS("my-project")`

**BrowserFS** is more full-featured with many backends (IndexedDB, in-memory, ZIP mounting, overlay filesystems, HTTP proxying) but heavier.

### Recommendation for Lexicon

A layered approach:
1. **Primary**: OPFS for persistent storage (fast, synchronous in Workers, no permissions)
2. **Fallback**: IndexedDB via LightningFS or similar for browsers without OPFS support
3. **Local files**: File System Access API for opening real local directories (Chromium only)
4. **In-memory**: For temporary/scratch buffers

### Feasibility Rating: **Ready now**

OPFS and IndexedDB-backed filesystems are production-ready. VS Code web proves the architecture works at scale.

---

## 4. WASM Shell/Process Model

### Browsix: Unix Processes in the Browser

[Browsix](https://browsix.org/) ([paper](https://arxiv.org/abs/1611.07862), ASPLOS 2017, [GitHub](https://github.com/plasma-umass/browsix)) is the seminal project for running Unix-like processes in the browser:

- **Architecture**: A TypeScript "kernel" runs on the main thread. Each "process" runs in a Web Worker.
- **System calls**: The process sends a message via `postMessage` with syscall number and arguments, then calls `Atomics.wait()` to block. The kernel processes the syscall, writes results into SharedArrayBuffer, and calls `Atomics.notify()` to wake the process.
- **SharedArrayBuffer**: Each process's heap is backed by a SharedArrayBuffer, enabling zero-copy data sharing with the kernel.
- **Capabilities**: `fork`, `spawn`, `exec`, `wait`, signals, pipes, sockets (TCP client/server), shared filesystem.
- **Languages**: Runs unmodified C, C++, Go, and Node.js programs compiled with Emscripten.
- **Status**: **No longer actively maintained** (last commit 2019). However, the architectural ideas remain highly influential. The paper has been cited by 2025 research including "Empowering WebAssembly with Thin Kernel Interfaces."

### Wasmer's Browser Shell

[WebAssembly.sh](https://webassembly.sh/) ([GitHub](https://github.com/wasmerio/webassembly.sh)) is a PWA terminal powered by Wasmer-JS that runs WASI/WASIX modules:
- Uses xterm.js for the terminal frontend
- Loads `sharrattj/bash` from the Wasmer registry -- a WASIX-compiled version of Bash with core Unix utilities
- Connects stdin/stdout/stderr of the Bash instance to xterm.js
- Supports piping between commands
- [Tutorial for building your own](https://docs.wasmer.io/sdk/wasmer-js/tutorials/xterm-js/)

### Web Workers as Processes

The key pattern from [MDN SharedArrayBuffer docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) and [Tweag's blog](https://www.tweag.io/blog/2022-11-24-wasm-threads-and-messages/):
- Web Workers are by default closer to a multi-**process** model (no shared state)
- With SharedArrayBuffer, you can opt into a more thread-like experience
- `Atomics.wait()` / `Atomics.notify()` enable synchronous blocking, which is essential for emulating blocking I/O (stdin reads, etc.)
- **Requirement**: Cross-origin isolation headers (`Cross-Origin-Opener-Policy: same-origin`, `Cross-Origin-Embedder-Policy: require-corp`)

### v86: Full x86 Emulation

[v86](https://github.com/copy/v86) runs an entire x86 PC in the browser via x86-to-WASM JIT compilation:
- Boots real Linux kernels (Alpine Linux images can be built from Dockerfiles)
- Uses xterm.js for terminal
- In November 2025, [Linux was ported to compile directly to WASM](https://linux.slashdot.org/story/25/11/03/0610234/linux-ported-to-webassembly-boots-in-a-browser-tab) -- kernel patches implement the Wasm architecture, allowing the kernel itself to be a WASM module
- This is a proof of concept, not practical for production shell use (significant overhead)

### Terminal Emulators

- **xterm.js** ([xtermjs.org](https://xtermjs.org/)): The dominant browser terminal emulator, used by VS Code, Wasmer, and many others
- **wterm** (Vercel Labs): Core written in Zig, compiled to WASM. Ships a ~12KB `.wasm` binary. Renders into DOM for native text selection and accessibility. Smaller ecosystem than xterm.js.
- **wasm-webterm** ([GitHub](https://github.com/cryptool-org/wasm-webterm)): xterm.js addon for running WASI + Emscripten binaries
- **workflow-terminal** ([docs.rs](https://docs.rs/crate/workflow-terminal/latest)): Rust library abstracting over xterm.js and termion for apps that work in both browser and native console

### Feasibility Rating: **Feasible with effort**

The Browsix architecture (Web Workers + SharedArrayBuffer + Atomics) is proven for Unix-like process emulation. Wasmer's browser shell proves that real Bash can run in a browser. The main effort is building/adapting the kernel and syscall layer for Lexicon's specific needs.

---

## 5. Existing WASM Command-Line Tool Projects

### WebContainers (StackBlitz)

[WebContainers](https://webcontainers.io/) ([announcement](https://blog.stackblitz.com/posts/introducing-webcontainers/)) are the most ambitious browser-based execution environment:
- Run full Node.js, npm/pnpm/yarn entirely in-browser
- Architecture: WASM-based OS layer + Service Workers + SharedArrayBuffer + cross-origin isolation
- Package installs 5x faster than native yarn/npm; builds 20% faster
- User code runs in Web Workers to avoid blocking UI
- [WASI integration announced](https://blog.stackblitz.com/posts/announcing-wasi/) -- enables running code compiled from any language to WASM
- Experimental Python support via Pyodide
- Security: All execution in browser sandbox, no remote VMs
- Browser support: Full in Chrome/Chromium; beta in Firefox/Safari
- **Proprietary** -- the WebContainers API is available but the core technology is not open-source

### WASI State of Affairs

**WASI 0.2** (formerly Preview 2) -- [status overview](https://eunomia.dev/blog/2025/02/16/wasi-and-the-webassembly-component-model-current-status/):
- Stabilized late 2024, widely adopted through 2025
- Includes: wasi-filesystem, wasi-io, wasi-http, wasi-sockets, wasi-clocks, wasi-random
- Component Model for composing modules from different languages
- Fully supported by Wasmtime, Spin, wasmCloud
- **Primarily for non-browser environments** (server-side, edge)

**WASI 0.3** -- [The New Stack](https://thenewstack.io/wasi-preview-2-what-webassembly-can-and-cant-do-yet/):
- Adds native async I/O
- Experimental support in Wasmtime
- First RC support in Spin v3.5 (November 2025)
- Expected stable release late 2026

**Key gaps** ([Java Code Geeks analysis](https://www.javacodegeeks.com/2026/04/webassembly-in-2026-three-years-of-almost-ready.html)):
- No multi-threading (Threads proposal in Phase 4 for core WASM spec)
- No `fork()` -- conflicts with Component Model philosophy
- No signal handling
- No full pthreads

### Emscripten vs WASI for Browser Targets

| Aspect | Emscripten | WASI |
|--------|-----------|------|
| Primary target | Browser | Server-side runtimes |
| Filesystem | Full virtual FS (MEMFS, IDBFS, etc.) | wasi-filesystem (host-provided) |
| Browser support | Native, production-proven | Requires shim/polyfill |
| Threading | pthreads via Web Workers | Not yet standardized |
| DOM access | Yes (via JS glue) | No |
| Maturity | 10+ years | 2-3 years |

For Lexicon's browser use case, **Emscripten is the more practical choice** for compiling C/C++ code. For Rust code, `wasm-bindgen` + `wasm32-unknown-unknown` target is the standard path, with WASI shims available for code that needs filesystem abstractions.

### Feasibility Rating: Mixed

- WebContainers: Not applicable (proprietary, Node.js focused)
- WASI browser polyfills: **Feasible with effort** for simple use cases
- Emscripten: **Ready now** for C/C++ compilation

---

## 6. tree-sitter WASM (web-tree-sitter)

### Current State

[web-tree-sitter](https://www.npmjs.com/package/web-tree-sitter) ([GitHub README](https://github.com/tree-sitter/tree-sitter/blob/master/lib/binding_web/README.md)) is the standard way to use tree-sitter in the browser:

- Tree-sitter's core runtime compiles to a ~252KB WASM module
- Language grammars compile to individual `.wasm` files, loaded on demand
- Since v0.26.1, `tree-sitter build --wasm` uses wasi-sdk and auto-downloads it
- Can be loaded standalone or bundled with Webpack/Vite

### Grammar .wasm File Sizes

Based on available data ([Issue #410](https://github.com/tree-sitter/tree-sitter/issues/410)):

| Language | Approx. WASM Size |
|----------|-------------------|
| tree-sitter runtime | ~252 KB |
| JavaScript | ~616 KB |
| Python | ~449 KB |
| Typical grammar | 200-800 KB |

The issue noted these sizes may be "unnecessarily bloated" and suggested optimization via function export whitelisting.

### How GitHub Uses It

GitHub uses tree-sitter for [syntax highlighting](https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html) and code navigation (jump-to-definition, symbol finding) in the web code viewer. Grammars are more concise than TextMate equivalents: tree-sitter-javascript/grammar.js is 1156 lines vs. VS Code's JavaScript TextMate grammar at 3500+ lines.

### Key Properties for Lexicon

- **Incremental parsing**: Re-parses only changed portions of a file. Fast enough for per-keystroke updates. ([Strumenta blog](https://tomassetti.me/incremental-parsing-using-tree-sitter/))
- **Error-tolerant**: Grammars designed to function gracefully during mid-edit states.
- **Manual memory management**: WASM objects (trees, nodes) must be explicitly freed -- no GC. ([Pulsar blog](https://blog.pulsar-edit.dev/posts/20240902-savetheclocktower-modern-tree-sitter-part-7/))
- **Emscripten version sensitivity**: Building .wasm files requires specific Emscripten versions matching the tree-sitter version.

### Lezer as Alternative

[Lezer](https://lezer.codemirror.net/) ([comparison discussion](https://discuss.codemirror.net/t/question-difference-between-lezer-and-tree-sitter/3115)), created by CodeMirror 6's author:
- Pure JavaScript -- no WASM overhead, no async initialization
- Smaller bundles, more compact in-memory trees
- Zero-dependency LR parsers generated from grammar files
- Integrated natively with CodeMirror 6
- Smaller grammar ecosystem than tree-sitter
- Not as error-tolerant as tree-sitter in some cases

**For Lexicon's Emacs compatibility goals**, tree-sitter is the better choice because:
1. Emacs 29+ has native tree-sitter integration (`treesit.el`)
2. The grammar ecosystem is much larger (305+ languages via [tree-sitter-language-pack](https://github.com/kreuzberg-dev/tree-sitter-language-pack))
3. Lexicon's Rust/WASM core already uses compiled code, so another WASM module is architecturally consistent

VS Code also uses tree-sitter in WASM via their [`@vscode/tree-sitter-wasm`](https://www.npmjs.com/package/@vscode/tree-sitter-wasm) package.

### Feasibility Rating: **Ready now**

web-tree-sitter is production-proven, used by GitHub.com and VS Code web. Grammar .wasm files are small enough for on-demand loading. Incremental parsing handles real-time editing.

---

## 7. Feasibility Assessment Summary

| Capability | Rating | Notes |
|-----------|--------|-------|
| **ripgrep search in WASM** | Feasible with effort | Core search works; needs JS-side file discovery |
| **find/fd in WASM** | Ready now (reimpl) | Reimplement in JS against virtual FS; no WASM needed |
| **Virtual filesystem** | Ready now | OPFS + IndexedDB; proven by VS Code web |
| **File System Access API** | Ready now (Chromium) | Local file access; Firefox/Safari unsupported |
| **Process model** | Feasible with effort | Browsix architecture proven; Wasmer shell runs Bash |
| **Shell in browser** | Feasible with effort | Wasmer's WASIX Bash works; integration is the work |
| **tree-sitter WASM** | Ready now | Production-proven; GitHub and VS Code use it |
| **Multi-module shared FS** | Feasible with effort | PROXYFS or IDBFS sync; WasmFS coming |
| **WASI in browser** | Research needed | WASI 0.2 is server-focused; browser polyfills exist but immature |
| **Full Linux in browser** | Not feasible (practical) | v86 proves it's possible but too heavy for production use |

---

## 8. Modular Subproject Potential

### A. Browser-native ripgrep library

**Value proposition**: A standalone npm package that exposes ripgrep's search engine as a WASM module with a clean JavaScript API (`search(pattern, bytes) -> matches`). Useful for any browser-based editor, documentation search, or code review tool.

**Standalone value**: High. Many web-based editors (Monaco, CodeMirror) and documentation sites could use fast regex search. Netgrep proves the concept; a polished, maintained package would fill a real gap.

**Effort**: Medium. The WASM compilation is proven. The work is in API design, size optimization, and packaging.

### B. WASM virtual filesystem

**Value proposition**: A unified browser filesystem abstraction with pluggable backends (OPFS, IndexedDB, File System Access API, in-memory) and a Node.js-compatible `fs` API.

**Standalone value**: Very high. Every browser-based IDE, notebook, or file manager needs this. LightningFS exists but is minimal; BrowserFS is unmaintained. A modern implementation using OPFS as primary backend with good WASM integration would be widely adopted.

**Effort**: Medium-high. The individual backends exist; the value is in the unified abstraction, caching strategy, and multi-tab synchronization.

### C. Browser process model

**Value proposition**: A lightweight "kernel" that maps Unix process semantics onto Web Workers, with stdin/stdout/stderr, pipes, signals, and a shared filesystem.

**Standalone value**: High but niche. Primarily valuable for browser-based IDEs and educational platforms. Browsix proved the concept but is abandoned. A modern TypeScript implementation using current APIs (OPFS, SharedArrayBuffer, Atomics) would be welcomed by the community.

**Effort**: High. This is essentially rebuilding Browsix with modern APIs.

### D. WASM shell runtime

**Value proposition**: A POSIX-compatible shell (Bash or dash) running in the browser with core Unix utilities, backed by a virtual filesystem.

**Standalone value**: High. WebAssembly.sh exists but is tied to Wasmer's ecosystem. A standalone, embeddable shell component with xterm.js integration would serve many use cases: interactive tutorials, cloud IDEs, educational platforms.

**Effort**: Medium (if building on Wasmer's WASIX Bash) to Very High (if building from scratch).

---

## Key Strategic Recommendations for Lexicon

1. **Start with tree-sitter WASM** -- it is ready today and directly enables Emacs-compatible syntax features. This is the lowest-risk, highest-reward integration.

2. **Build the virtual filesystem layer early** -- using OPFS as primary backend with IndexedDB fallback. This is foundational for everything else (grep, find, shell). Consider building on top of or forking LightningFS.

3. **Implement `consult-find` in pure JavaScript/ClojureScript** against the virtual filesystem. No WASM needed for file discovery.

4. **Implement `consult-ripgrep` using ripgrep's grep crates compiled to WASM** as a search engine, with JavaScript handling file enumeration and feeding byte slices to the WASM module. The Netgrep project provides a reference architecture.

5. **Defer the shell/process model** -- this is the highest-effort item and least critical for an editor. When ready, the Wasmer WASIX approach (embedding a pre-compiled Bash) is more practical than building from scratch.

6. **Watch WASI 0.3** -- native async I/O (expected late 2026) will significantly improve the browser WASM story for tools that need I/O.

---

Sources:
- [Netgrep - ripgrep WASM port](https://github.com/dgopsq/netgrep)
- [Diego Pasquali - ripgrep-powered Search Engine on the web](https://diegopasquali.com/blog/ripgrep-powered-search-engine-on-the-web)
- [ripgrep npm package (WASM)](https://www.npmjs.com/package/ripgrep)
- [BurntSushi/ripgrep Discussion #2067](https://github.com/BurntSushi/ripgrep/discussions/2067)
- [Large WASM builds with Rust and regex](https://esimmler.com/large-wasm-builds-with-rust-regex)
- [sharkdp/fd](https://github.com/sharkdp/fd)
- [Emscripten File System API](https://emscripten.org/docs/api_reference/Filesystem-API.html)
- [Emscripten File System Overview](https://emscripten.org/docs/porting/files/file_systems_overview.html)
- [WasmFS OPFS performance issue](https://github.com/emscripten-core/emscripten/issues/24639)
- [Origin Private File System (MDN)](https://developer.mozilla.org/en-US/docs/Web/API/File_System_API/Origin_private_file_system)
- [OPFS article (web.dev)](https://web.dev/articles/origin-private-file-system)
- [SQLite WASM with OPFS (Chrome blog)](https://developer.chrome.com/blog/sqlite-wasm-in-the-browser-backed-by-the-origin-private-file-system)
- [File System Access API (Chrome Developers)](https://developer.chrome.com/docs/capabilities/web-apis/file-system-access)
- [VS Code File System Access API](https://nasserspace.hashnode.dev/vscode-file-system-access-api)
- [VS Code Virtual Workspaces](https://code.visualstudio.com/api/extension-guides/virtual-workspaces)
- [LightningFS](https://github.com/isomorphic-git/lightning-fs)
- [Browsix](https://browsix.org/)
- [Browsix GitHub](https://github.com/plasma-umass/browsix)
- [SharedArrayBuffer (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer)
- [Threads and messages with Rust and WASM (Tweag)](https://www.tweag.io/blog/2022-11-24-wasm-threads-and-messages/)
- [SharedArrayBuffer and WASM (ddr0.ca)](https://ddr0.ca/blog-posts/13.Shared_Array_Buffers_With_WASM)
- [WebContainers](https://webcontainers.io/)
- [WebContainers WASI announcement](https://blog.stackblitz.com/posts/announcing-wasi/)
- [Wasmer xterm.js tutorial](https://docs.wasmer.io/sdk/wasmer-js/tutorials/xterm-js/)
- [WebAssembly.sh](https://webassembly.sh/)
- [wasm-webterm (xterm.js addon)](https://github.com/cryptool-org/wasm-webterm)
- [v86 x86 emulator](https://github.com/copy/v86)
- [Linux ported to WebAssembly](https://linux.slashdot.org/story/25/11/03/0610234/linux-ported-to-webassembly-boots-in-a-browser-tab)
- [xterm.js](https://xtermjs.org/)
- [web-tree-sitter (npm)](https://www.npmjs.com/package/web-tree-sitter)
- [tree-sitter GitHub](https://github.com/tree-sitter/tree-sitter)
- [tree-sitter WASM size optimization (Issue #410)](https://github.com/tree-sitter/tree-sitter/issues/410)
- [tree-sitter web README](https://github.com/tree-sitter/tree-sitter/blob/master/lib/binding_web/README.md)
- [Modern Tree-sitter part 7 (Pulsar blog)](https://blog.pulsar-edit.dev/posts/20240902-savetheclocktower-modern-tree-sitter-part-7/)
- [Lezer System Guide](https://lezer.codemirror.net/docs/guide/)
- [Lezer vs tree-sitter discussion](https://discuss.codemirror.net/t/question-difference-between-lezer-and-tree-sitter/3115)
- [tree-sitter-language-pack (305+ languages)](https://github.com/kreuzberg-dev/tree-sitter-language-pack)
- [@vscode/tree-sitter-wasm](https://www.npmjs.com/package/@vscode/tree-sitter-wasm)
- [ast-grep WASM](https://lib.rs/crates/ast-grep-wasm)
- [WASI current status (eunomia)](https://eunomia.dev/blog/2025/02/16/wasi-and-the-webassembly-component-model-current-status/)
- [WASI Preview 2 (The New Stack)](https://thenewstack.io/wasi-preview-2-what-webassembly-can-and-cant-do-yet/)
- [WebAssembly in 2026 (Java Code Geeks)](https://www.javacodegeeks.com/2026/04/webassembly-in-2026-three-years-of-almost-ready.html)
- [State of WebAssembly 2025-2026 (Uno Platform)](https://platform.uno/blog/the-state-of-webassembly-2025-2026/)
- [Syntax highlighting on the web (Joel Gustafson)](https://joelgustafson.com/posts/2022-05-31/syntax-highlighting-on-the-web/)
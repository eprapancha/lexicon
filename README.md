# **Lexicon: A Modern, Browser-Based Editor**

Lexicon is an architectural exploration into building a highly extensible, keyboard-driven code editor for the modern web. Our mission is to capture the spirit of GNU Emacs—its malleability, introspection, and legendary extensibility—while building upon a foundation of modern, high-performance, and principled software architecture.

This project is a deep dive into editor design, synthesizing the timeless architectural patterns of Emacs with the functional, state-of-the-art paradigms of systems like CodeMirror 6. The result is an editor designed to be powerful, performant, and a joy to develop with.

## **Current Status: The Extensible Foundation is Live**

Lexicon has successfully evolved from a concept to a **working, extensible editor** with core Emacs-like capabilities. The editor now features:

- ✅ **A high-performance Rust/WASM text engine** with piece tree data structures
- ✅ **Multi-buffer and multi-window management** with persistent file operations
- ✅ **Complete Emacs-style command and keymap system** with hierarchical precedence
- ✅ **Major and minor mode architecture** with hooks for extensibility
- ✅ **Interactive command execution** via both keybindings and programmatic dispatch
- ✅ **Professional development environment** with reproducible builds

**You can now use Lexicon as a functional text editor with Emacs-style keybindings like `C-x C-f` to open files and `C-x C-s` to save!**

## **Core Philosophy: A Synthesis of Classic and Modern**

Lexicon is built upon a clear and robust set of architectural principles that guide every implementation decision:

1. **The "Functional Core, Imperative Shell" Model**: Inspired by CodeMirror 6, the editor's state is managed by a pure, functional core using immutable data structures and atomic, transaction-based updates. This creates a predictable, unidirectional data flow, which is essential for complex features like a reliable undo history and real-time collaboration. The view layer acts as an "imperative shell," handling the messy realities of DOM interaction while remaining a pure reflection of the canonical state.

2. **The "Userspace/Kernel" Split**: Drawing from the wisdom of Emacs's design, Lexicon is bifurcated into two distinct realms:

   * **The ClojureScript "Userspace"**: This is the interactive heart of the editor, analogous to the Emacs Lisp environment. Built with the **re-frame** framework, it handles all UI, command logic, and the user-facing extensibility system in a structured, scalable way.  
   * **The Rust/WASM "Kernel"**: This is the high-performance core, analogous to Emacs's C kernel. Written in **Rust** and compiled to WebAssembly, it manages the most computationally intensive text manipulation tasks, ensuring a responsive user experience.

3. **A Professional, Standardized Development Environment**: To eliminate the "it works on my machine" problem and streamline onboarding, the entire development environment is codified and version-controlled using **Nix** and structured scripts. This ensures every developer, as well as our CI pipeline, uses an identical, reproducible toolchain.

## **The Technology Stack**

Lexicon is a polyglot application, choosing the best tool for each architectural layer. The monorepo structure reflects this clear separation of concerns.

* **UI & Extensibility Layer (`editor-cljs`)**: **ClojureScript** with the **re-frame** framework provides a powerful, REPL-driven environment for building a dynamic and extensible UI.  
* **High-Performance Core (`lexicon-engine`)**: **Rust**, compiled to **WebAssembly (WASM)**, is used for the performance-critical text engine.  
  * **Core Data Structure**: A **Piece Tree** provides O(log k) performance for all text manipulations and superior memory efficiency.  
  * **Memory Management**: Leverages the browser's native garbage collection for reduced complexity.
  * **Architecture**: The engine is structured using the **"library-first" pattern**, separating the pure `lexicon-core` library from the `lexicon-wasm` bindings for enhanced testability and reusability.  
* **Syntax Parsing (`language-grammars`)**: **Tree-sitter** provides fast, incremental, and error-tolerant parsing. Grammars are compiled to WASM and run in a Web Worker to keep the UI fully responsive.  
* **Backend Services (`backend-server`)**: **Clojure** powers the local companion server, which will bridge the browser sandbox to enable interaction with native developer tools like the Language Server Protocol (LSP).

## **The Grand Vision: An Incremental Roadmap**

We are building Lexicon through a series of well-defined, incremental phases. Each phase delivers a more capable editor, building upon the robust foundation of the last.

#### **L1.1: The Core Engine & View Layer MVP ✅ COMPLETE**

* **Goal**: To build a functional "Typing Machine" that validates the entire architecture, from user input to the WASM kernel and back to the DOM.  
* **Status**: **COMPLETE**. The editor now has a solid, performant, and future-proof foundation. All core tasks are finished, including the Piece Tree implementation, the transaction-based state model, and the proactive `contenteditable` view layer. We have also completed a critical "library-first" refactor of the Rust engine to align with best practices.
* **Key Achievements**:
  * ✅ Rust/WASM text engine with piece tree data structure
  * ✅ ClojureScript UI with re-frame state management
  * ✅ Transaction-based text operations with undo/redo
  * ✅ DOM reconciliation and cursor management
  * ✅ Performance-optimized rendering with virtualization

#### **L1.2: The "Functional Editor" ✅ COMPLETE**

* **Goal**: To build upon the core by adding essential Emacs-like editing features.  
* **Status**: **COMPLETE**. Lexicon now functions as a capable multi-buffer text editor with persistent file operations.
* **Key Achievements**:  
  * ✅ Multi-buffer and multi-window state management
  * ✅ Local file system access via the File System Access API
  * ✅ The core kill ring (clipboard history) and region-based commands
  * ✅ Buffer tabs with modified indicators and close functionality
  * ✅ Status bar with cursor position and buffer information
  * ✅ File save/load with proper error handling

#### **L1.3: The "Extensible Emacs" Foundation ✅ COMPLETE**

* **Goal**: To build the core machinery that enables Emacs-like extensibility.  
* **Status**: **COMPLETE**. Lexicon now has a complete command and keymap system with mode support.
* **Key Achievements**:  
  * ✅ Central command dispatcher with dynamic command registration
  * ✅ Hierarchical keymap system with Emacs-style precedence (minor → major → global)
  * ✅ Multi-key sequence support (e.g., `C-x C-f`)
  * ✅ Major and minor mode architecture with buffer-local state
  * ✅ Hook system for intercepting and customizing behavior
  * ✅ Interactive command execution via keybindings
  * ✅ Built-in commands: `find-file`, `save-buffer`, `kill-region`, `yank`, etc.

#### **L1.4: The "Robust Text Foundation" ✅ COMPLETE**

* **Goal**: To eliminate cursor race conditions and establish a rock-solid text editing foundation.  
* **Status**: **COMPLETE**. Lexicon now has a production-ready text editing architecture that handles fast typing without race conditions.
* **Key Achievements**:  
  * ✅ **Hidden Textarea + Custom DOM architecture** - Industry standard pattern for reliable input capture
  * ✅ **Cursor as first-class citizen** - Application-managed cursor eliminates browser conflicts  
  * ✅ **Transaction queue system** - Serialized text operations prevent race conditions during fast typing
  * ✅ **Coordinate conversion system** - Seamless translation between linear positions and line/column coordinates
  * ✅ **Custom cursor rendering** - CSS-animated cursor positioned by application state
  * ✅ **Race condition elimination** - Robust transaction processing prevents text corruption

#### **L1.5: Security Hardening & Modal Editing Foundation ✅ COMPLETE**

* **Goal**: Critical security hardening of bridge infrastructure and implementation of modal editing foundation.
* **Status**: **COMPLETE**. Production-ready security measures implemented alongside comprehensive modal editing architecture.
* **Key Achievements**:
  * ✅ **Critical Security Hardening** - Bridge server now production-ready with comprehensive security measures
    * ✅ Command injection mitigation using `child_process.spawn()` with argument arrays
    * ✅ Input validation and language ID whitelisting 
    * ✅ Ticket-based WebSocket authentication system with TTL expiration
    * ✅ Enhanced logging with tier-of-origin classification (`:client`, `:bridge-server`, `:lsp-process`)
  * ✅ **Modal Editing Foundation (Phase L2.1)** - Complete FSM-based architecture for Vim/Evil-style editing
    * ✅ Pure FSM state management with comprehensive modal state schema
    * ✅ Metaprogramming DSL with `def-evil-motion`, `def-evil-operator`, `def-evil-text-object` macros
    * ✅ Operator-motion composition engine implementing Vim's "verb-noun" grammar
    * ✅ Centralized keymap registry with context-aware precedence resolution
    * ✅ State lifecycle hooks via re-frame interceptors (on-enter/on-exit)

#### **L1.6: The "Modern Powerhouse" 🔄 NEXT**

* **Goal**: To integrate advanced, language-aware editing features with secure LSP bridge.
* **Key Features**:  
  * 🔲 Asynchronous, incremental parsing with **Tree-sitter** in a Web Worker
  * 🔲 Declarative, query-based syntax highlighting and code folding
  * 🔲 Language-aware editing commands (smart indentation, navigation)
  * 🔲 Production LSP integration with security-hardened bridge

#### **L1.7: The System-Integrated IDE**

* **Goal**: To break out of the browser sandbox by connecting to native developer tools.  
* **Key Features**:  
  * ✅ The `lexicon-bridge`, a secure local companion process that bridges the browser to the OS via WebSockets
  * 🔲 Full Language Server Protocol (LSP) integration for features like diagnostics, code completion, and go-to-definition
  * 🔲 Integration with external tools (git, linters, formatters)

#### **L1.8: The Collaborative Cloud IDE**

* **Goal**: To evolve Lexicon into a full-fledged, multiuser, cloud-native development environment.  
* **Key Features**:  
  * 🔲 A backend orchestration service to manage containerized workspaces
  * 🔲 "Workspace as Code" via `devcontainer.json` file support
  * 🔲 Real-time collaborative editing with conflict resolution

## **Getting Started**

### Prerequisites
- **Nix** (for reproducible development environment)
- **Modern browser** with WebAssembly and File System Access API support
- **Node.js** and **npm** (for bridge server and development)

### Quick Start
```bash
# Clone the repository
git clone <repository-url>
cd lexicon

# Start the development environment
npm run dev

# In a separate terminal, start the secure bridge server
cd packages/lexicon-bridge
node index.js

# Open your browser to http://localhost:8080
```

## **Available Commands and Functionality**

### Core Editor Features (Currently Working)
- ✅ **Multi-buffer text editing** with persistent file operations
- ✅ **Emacs-style keyboard shortcuts** and command system
- ✅ **Kill ring (clipboard history)** with region-based operations
- ✅ **Buffer tabs** with modified indicators
- ✅ **Status bar** with cursor position and buffer information
- ✅ **Transaction-based undo/redo** system
- ✅ **Race-condition-free input handling**

### Security Infrastructure (Production Ready)
- ✅ **Secure WebSocket bridge** with ticket-based authentication
- ✅ **Command injection protection** via process spawning safeguards
- ✅ **Input validation** and language whitelisting
- ✅ **Comprehensive logging** with security audit trails

### Modal Editing Foundation (Ready for Implementation)
- ✅ **FSM state management** for Normal/Insert/Visual modes
- ✅ **Operator-motion composition** (Vim's "verb-noun" grammar)
- ✅ **Metaprogramming macros** for defining Evil-style commands
- ✅ **Context-aware keymap system** with precedence resolution
- ✅ **State lifecycle hooks** for mode transitions

### Keyboard Shortcuts
**Emacs-style Navigation:**
- `Ctrl+X Ctrl+F` - Open file (find-file)
- `Ctrl+X Ctrl+S` - Save file (save-buffer)
- `Ctrl+G` - Cancel operation
- `Ctrl+W` - Kill region (cut)
- `Ctrl+Y` - Yank (paste from kill ring)
- `Ctrl+Space` - Set mark (begin selection)
- `Alt+W` - Copy region to kill ring

**Buffer Management:**
- `Ctrl+X B` - Switch buffer
- `Ctrl+X K` - Kill buffer
- `Ctrl+X 1` - Delete other windows
- `Ctrl+X 2` - Split window horizontally

### Programmatic API
**Execute Commands:**
```javascript
// Execute built-in commands
window.re_frame.core.dispatch(['execute-command', 'find-file']);
window.re_frame.core.dispatch(['execute-command', 'save-buffer']);

// FSM state transitions (modal editing)
window.re_frame.core.dispatch(['fsm/transition-to', 'normal']);
window.re_frame.core.dispatch(['fsm/transition-to', 'insert']);
```

**Register Custom Commands:**
```javascript
// Register new commands
window.re_frame.core.dispatch(['register-command', 'my-command', {
  docstring: "My custom command",
  handler: ['show-message', 'Hello from custom command!']
}]);
```

**Modal Editing Macros (ClojureScript):**
```clojure
;; Define Evil-style motions
(def-evil-motion word-forward [count]
  "Move forward by word"
  {:repeatable true :jump-list true}
  (move-by-word count))

;; Define Evil-style operators  
(def-evil-operator delete-operator [motion]
  "Delete text specified by motion"
  {:needs-motion true}
  (delete-range (motion)))
```

### Bridge Server Features
**Security:**
- ✅ **Ticket-based WebSocket authentication** (port 30304)
- ✅ **Secure process spawning** with argument validation
- ✅ **CORS protection** for localhost origins only
- ✅ **Request logging** with security classifications

**LSP Integration (Ready):**
- ✅ **Language server process management**
- ✅ **JSON-RPC message forwarding**
- ✅ **Multi-client support** per language server
- ✅ **Graceful process cleanup**

### Development Commands
```bash
# Build and test the editor
npm run build:editor

# Run ClojureScript compilation
npm run cljs:compile

# Build Rust/WASM engine
npm run build:engine

# Start bridge server with debugging
cd packages/lexicon-bridge && node index.js

# Run tests (when available)
npm test
```

## **Architecture Overview**

Lexicon's architecture reflects its multi-layered approach: a security-hardened bridge, FSM-based modal editing, and high-performance text engine.

```
┌─────────────────────────────────────────┐
│        Browser (localhost:8080)         │
│     ┌─────────────────────────────────┐ │
│     │     ClojureScript UI            │ │
│     │   (re-frame + Reagent)          │ │
│     ├─────────────────────────────────┤ │
│     │   FSM Modal Editing System     │ │
│     │ (Evil/Vim-style architecture)  │ │
│     ├─────────────────────────────────┤ │
│     │   Command & Keymap System      │ │
│     │  (Emacs-style extensibility)   │ │
│     ├─────────────────────────────────┤ │
│     │  Hidden Textarea + Custom DOM  │ │
│     │   (Race-condition-free input)  │ │
│     ├─────────────────────────────────┤ │
│     │   Transaction Queue System     │ │
│     │  (Serialized text operations)  │ │
│     ├─────────────────────────────────┤ │
│     │      WebAssembly API           │ │
│     ├─────────────────────────────────┤ │
│     │     Rust Text Engine           │ │
│     │    (Piece tree + UTF-8)        │ │
│     └─────────────────────────────────┘ │
└─────────────────────────────────────────┘
            │ WebSocket + HTTPS
            │ (Ticket Authentication)
┌─────────────────────────────────────────┐
│     Lexicon Bridge (localhost:30303)    │
│   ┌─────────────────────────────────┐   │
│   │   Security Layer               │   │
│   │ • Ticket-based auth            │   │
│   │ • Input validation             │   │
│   │ • Command injection protection │   │
│   ├─────────────────────────────────┤   │
│   │   LSP Process Management       │   │
│   │ • Multi-language support       │   │
│   │ • JSON-RPC forwarding          │   │
│   │ • Process lifecycle            │   │
│   └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
            │ Native Process Spawning
┌─────────────────────────────────────────┐
│        Language Servers                 │
│  • typescript-language-server          │
│  • rust-analyzer                       │
│  • clojure-lsp                          │
│  • pylsp                                │
└─────────────────────────────────────────┘
```

## **Contributing**

Lexicon is designed to be hackable and extensible. Whether you're interested in:
- 🦀 **Rust/WASM performance optimization**
- 🔧 **ClojureScript UI development** 
- ⌨️ **Emacs-style extensibility features**
- 📝 **Language grammar development**

There are opportunities to contribute at every level of the stack.

## **Project Structure**

```
lexicon/
├── packages/
│   ├── editor-cljs/          # ClojureScript UI and extensibility layer
│   │   ├── src/lexicon/
│   │   │   ├── db.cljs       # App state schema with FSM modal editing
│   │   │   ├── events.cljs   # Event handlers with security & modal editing
│   │   │   ├── macros.clj    # Evil-style metaprogramming DSL
│   │   │   ├── fsm/          # Modal editing FSM architecture
│   │   │   ├── command/      # Command dispatcher with operator-motion
│   │   │   └── keymaps/      # Context-aware keymap registry
│   ├── lexicon-engine/       # Rust/WASM text engine with piece tree
│   ├── lexicon-bridge/       # Security-hardened LSP bridge server
│   │   └── index.js          # WebSocket + HTTP server with auth
│   ├── language-grammars/    # Tree-sitter parsing grammars
│   └── backend-server/       # Future: Clojure orchestration server
├── scripts/                  # Development and build scripts
└── docs/                     # Architecture and API documentation
```

## **Security Considerations**

### Production-Ready Security Features
- ✅ **Command Injection Protection**: All process spawning uses `child_process.spawn()` with argument arrays
- ✅ **Input Validation**: Language IDs validated against whitelist before process spawning
- ✅ **Authentication**: Ticket-based WebSocket authentication with TTL expiration (30s)
- ✅ **CORS Protection**: HTTP endpoints restricted to localhost origins only
- ✅ **Audit Logging**: Comprehensive request logging with tier-of-origin classification
- ✅ **Process Isolation**: Language servers run in separate processes with stdio piping
- ✅ **Resource Cleanup**: Graceful process termination and resource management

### Security Architecture
The lexicon-bridge operates as a **security-first** local development server:
1. **HTTP Ticket Endpoint** (port 30304): Issues time-limited authentication tickets
2. **WebSocket Bridge** (port 30303): Validates tickets before allowing connections
3. **Process Management**: Safely spawns and manages language server processes
4. **Request Validation**: All inputs validated and sanitized before processing

## **License**

MIT - See [LICENSE](LICENSE) for details.

---

*Lexicon represents a modern synthesis of time-tested editor design principles with cutting-edge web technologies. It's proof that the browser can be a first-class platform for serious software development tools.*
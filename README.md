# **Lexicon: A Modern, Browser-Based Editor**

Lexicon is an architectural exploration into building a highly extensible, keyboard-driven code editor for the modern web. Our mission is to capture the spirit of GNU Emacs—its malleability, introspection, and legendary extensibility—while building upon a foundation of modern, high-performance, and principled software architecture.

This project is a deep dive into editor design, synthesizing the timeless architectural patterns of Emacs with the functional, state-of-the-art paradigms of systems like CodeMirror 6\. The result is an editor designed to be powerful, performant, and a joy to develop with.

## **Core Philosophy: A Synthesis of Classic and Modern**

Lexicon is built upon a clear and robust set of architectural principles that guide every implementation decision:

1. **The "Functional Core, Imperative Shell" Model**: Inspired by CodeMirror 6, the editor's state is managed by a pure, functional core using immutable data structures and atomic, transaction-based updates. This creates a predictable, unidirectional data flow, which is essential for complex features like a reliable undo history and real-time collaboration. The view layer acts as an "imperative shell," handling the messy realities of DOM interaction while remaining a pure reflection of the canonical state.

2. **The "Userspace/Kernel" Split**: Drawing from the wisdom of Emacs's design, Lexicon is bifurcated into two distinct realms:

   * **The ClojureScript "Userspace"**: This is the interactive heart of the editor, analogous to the Emacs Lisp environment. Built with the **re-frame** framework, it handles all UI, command logic, and the user-facing extensibility system in a structured, scalable way.  
   * **The Rust/Wasm "Kernel"**: This is the high-performance core, analogous to Emacs's C kernel. Written in **Rust** and compiled to WebAssembly with **WasmGC**, it manages the most computationally intensive tasks, ensuring a responsive user experience.  
3. **A Professional, Standardized Development Environment**: To eliminate the "it works on my machine" problem and streamline onboarding, the entire development environment is codified and version-controlled using **Dev Containers**. This ensures every developer, as well as our CI pipeline, uses an identical, reproducible toolchain.

## **The Technology Stack**

Lexicon is a polyglot application, choosing the best tool for each architectural layer. The monorepo structure reflects this clear separation of concerns.

* **UI & Extensibility Layer (`editor-cljs`)**: **ClojureScript** with the **re-frame** framework provides a powerful, REPL-driven environment for building a dynamic and extensible UI.  
* **High-Performance Core (`lexicon-engine`)**: **Rust**, compiled to **WebAssembly (Wasm)**, is used for the performance-critical text engine.  
  * **Core Data Structure**: A **Piece Tree** provides O(log k) performance for all text manipulations and superior memory efficiency.  
  * **Memory Management**: We target **WasmGC**, leveraging the browser's native garbage collector to dramatically reduce complexity and risk.  
  * **Architecture**: The engine is structured using the **"library-first" pattern**, separating the pure `lexicon-core` library from the `lexicon-wasm` bindings for enhanced testability and reusability.  
* **Syntax Parsing (`language-grammars`)**: **Tree-sitter** provides fast, incremental, and error-tolerant parsing. Grammars are compiled to Wasm and run in a Web Worker to keep the UI fully responsive.  
* **Backend Services (`backend-server`)**: **Clojure** powers the local companion server, which will bridge the browser sandbox to enable interaction with native developer tools like the Language Server Protocol (LSP).

## **The Grand Vision: An Incremental Roadmap**

We are building Lexicon through a series of well-defined, incremental phases. Each phase delivers a more capable editor, building upon the robust foundation of the last.

#### **L1.1: The Core Engine & View Layer MVP (Complete)**

* **Goal**: To build a functional "Typing Machine" that validates the entire architecture, from user input to the Wasm kernel and back to the DOM.  
* **Status**: **COMPLETE**. The editor now has a solid, performant, and future-proof foundation. All core tasks are finished, including the Piece Tree implementation, the transaction-based state model, and the proactive `contenteditable` view layer. We have also completed a critical "library-first" refactor of the Rust engine to align with best practices.

#### **L1.2: The "Functional Editor"**

* **Goal**: To build upon the core by adding essential Emacs-like editing features.  
* **Key Features**:  
  * Multi-buffer and multi-window state management.  
  * Local file system access via the File System Access API.  
  * The core kill ring (clipboard history) and region-based commands.

#### **L1.3: The "Extensible Emacs" Foundation**

* **Goal**: To build the core machinery that enables Emacs-like extensibility.  
* **Key Features**:  
  * A central command dispatcher and interactive minibuffer.  
  * A hierarchical keymap system that respects major and minor modes.  
  * A system of hooks for intercepting and customizing behavior.

#### **L1.4: The "Modern Powerhouse"**

* **Goal**: To integrate advanced, language-aware editing features.  
* **Key Features**:  
  * Asynchronous, incremental parsing with **Tree-sitter** in a Web Worker.  
  * Declarative, query-based syntax highlighting and code folding.

#### **L1.5: The System-Integrated IDE**

* **Goal**: To break out of the browser sandbox by connecting to native developer tools.  
* **Key Features**:  
  * The `backend-server`, a local companion process that bridges the browser to the OS via WebSockets.  
  * Full Language Server Protocol (LSP) integration for features like diagnostics, code completion, and go-to-definition.

#### **L1.6: The Collaborative Cloud IDE**

* **Goal**: To evolve Lexicon into a full-fledged, multiuser, cloud-native development environment.  
* **Key Features**:  
  * A backend orchestration service to manage containerized workspaces.  
  * "Workspace as Code" via `devcontainer.json` file support.  
  * Real-time collaborative editing.


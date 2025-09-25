

# **Lexicon: An Implementation Plan**

This document outlines the vision, architecture, and step-by-step implementation plan for **Lexicon**, a modern, browser-based code editor. It is designed to be a comprehensive guide for development, providing deep context for each architectural decision and component.

## **1\. Vision & Core Philosophy**

Lexicon aims to be a highly extensible, keyboard-driven code editor inspired by the power and flexibility of Emacs, but built from the ground up for the modern web.

Our guiding architectural principle is the **"Functional Core, Imperative Shell"** model, heavily influenced by CodeMirror 6\.1 This means:

* **Functional Core:** The editor's state, including the document itself, is managed by pure, immutable data structures. State changes are described by transactions, creating a predictable and traceable update cycle. This is crucial for complex features like collaborative editing and a robust undo history.3  
* **Imperative Shell:** The user-facing component, which interacts with the browser's DOM, acts as an imperative layer around the functional core. It translates user actions (keystrokes, mouse events) into transactions and updates the view in response to state changes.1

## **2\. Technology Stack**

Lexicon is a polyglot application, selecting the best tool for each specific task:

* **Editor Framework:** **CodeMirror 6** serves as the foundational library for the view and state management layers. Its modular, functional, and extensible design is a perfect match for our core philosophy.1  
* **High-Performance Core:** **AssemblyScript (compiled to WebAssembly)** is used for performance-critical components, such as the core text data structure and complex algorithms. This allows us to achieve near-native speed in the browser, avoiding JavaScript's garbage collection pauses for critical operations.5  
* **UI & Extensibility Layer:** **ClojureScript** is the primary language for the UI, editor functionality, and the entire extensibility system. Its Lisp heritage, immutable data structures, and powerful REPL-driven development workflow make it ideal for building an Emacs-like experience.  
* **Syntax Parsing:** **Tree-sitter** provides robust, incremental parsing for various languages. Grammars are compiled to WebAssembly and loaded at runtime, offering efficient and error-tolerant syntax analysis.6  
* **Backend Services:** **Clojure** is used for the server-side application, handling persistence, user accounts, and real-time collaboration logic.

## **3\. Architectural Overview**

The application is divided into four primary, independently manageable packages within a single monorepo.

* **core-wasm (AssemblyScript):** The functional heart of the editor. It contains the core data structures (e.g., a Rope for text storage) and algorithms, compiled to a highly optimized WebAssembly module. It has no knowledge of the DOM and exposes a pure API for text manipulation.  
* **editor-cljs (ClojureScript):** The front-end application and user interface. It initializes and controls the CodeMirror 6 view, translates user input into commands, interacts with the core-wasm module, and manages all editor features like keymaps, commands, and UI elements.  
* **backend-server (Clojure):** The server-side component. It handles tasks that require persistence or a centralized authority, such as saving files, managing user authentication, and orchestrating collaborative editing sessions.  
* **language-grammars (Tree-sitter):** A dedicated location for managing and compiling Tree-sitter language grammars into individual .wasm files that can be loaded on-demand by the editor.

## **4\. Repository Structure**

The project is organized as a monorepo to simplify dependency management and ensure all components evolve in sync.5

lexicon/  
├──.github/  
│   └── workflows/  
│       └── ci.yml  
├── docs/  
│   ├── architecture.md  
│   ├── development-setup.md  
│   └── api/  
├── packages/  
│   ├── core-wasm/  
│   │   ├── assembly/  
│   │   │   └── index.ts  
│   │   ├── build/  
│   │   ├── tests/  
│   │   ├── asconfig.json  
│   │   └── package.json  
│   │  
│   ├── editor-cljs/  
│   │   ├── resources/  
│   │   │   └── public/  
│   │   ├── src/  
│   │   │   └── lexicon/  
│   │   │       └── core.cljs  
│   │   ├── test/  
│   │   ├── deps.edn  
│   │   └── shadow-cljs.edn  
│   │  
│   ├── backend-server/  
│   │   ├── src/  
│   │   │   └── lexicon/  
│   │   │       └── server.clj  
│   │   ├── resources/  
│   │   │   └── config.edn  
│   │   ├── test/  
│   │   └── deps.edn  
│   │  
│   └── language-grammars/  
│       ├── javascript/  
│       │   ├── grammar.js  
│       │   └── package.json  
│       └── build-grammars.sh  
│  
├── scripts/  
│   ├── build.sh  
│   ├── dev.sh  
│   └── test.sh  
│  
├──.gitignore  
├── LICENSE  
├── package.json  
└── README.md

## **5\. Consolidated Implementation Plan**

This plan breaks down the development process into logical phases. Each phase builds upon the last, resulting in a fully-featured editor.

---

### **Phase 1: Project Scaffolding and Basic Editor**

**Goal:** Establish the project structure and render a minimal, functional CodeMirror 6 editor using ClojureScript.

1. **Initialize Monorepo:**  
   * Create the root lexicon/ directory.  
   * Initialize a package.json file at the root.  
   * Configure it to use NPM/Yarn/PNPM workspaces, pointing to the packages/\* directory.  
2. **Set Up editor-cljs Package:**  
   * Create the /packages/editor-cljs directory.  
   * Initialize a ClojureScript project with deps.edn and shadow-cljs.edn.  
   * Add CodeMirror 6 packages (@codemirror/state, @codemirror/view, codemirror) as NPM dependencies.  
3. **Render a Basic Editor:**  
   * In src/lexicon/core.cljs, write the necessary ClojureScript code to interop with the CodeMirror 6 API.  
   * Instantiate an EditorView with a basic EditorState.  
   * Mount the editor onto a DOM element defined in resources/public/index.html.  
4. **Configure Build Scripts:**  
   * In the root package.json, add a script that runs the shadow-cljs watch command for the editor-cljs package.  
   * Create the master scripts/dev.sh script to launch this process.  
   * **Checkpoint:** Running scripts/dev.sh should start the shadow-cljs build, and opening the browser should display a functional, albeit minimal, text editor.

---

### **Phase 2: High-Performance Core in WebAssembly**

**Goal:** Implement the functional core of the editor's text management in AssemblyScript, focusing on performance and memory control.

1. **Set Up core-wasm Package:**  
   * Create the /packages/core-wasm directory.  
   * Use npx asinit. to scaffold a new AssemblyScript project.7  
2. **Implement the Core Data Structure:**  
   * Choose and implement a text-storage data structure. A **Rope** is recommended for its efficiency with large files and frequent, non-local edits.8  
   * The Rope will be a binary tree where leaves are strings (text chunks) and internal nodes store metadata (like the length of their left subtree).12  
3. **Implement Custom Memory Management:**  
   * WebAssembly's linear memory requires manual management. To avoid the overhead and non-determinism of the default garbage collector, implement a custom memory allocator.13  
   * A **Slab Allocator** is a strong choice, as Rope nodes are fixed-size objects, which is the ideal use case for slab allocation.15 This minimizes fragmentation and makes allocation/deallocation extremely fast.19  
   * The allocator will manage the ArrayBuffer that constitutes the WASM module's memory.  
4. **Define the WASM Module API:**  
   * Export functions from AssemblyScript for the ClojureScript layer to call.  
   * Initial API:  
     * init(): To set up the initial empty text buffer.  
     * insert(pos, text): To insert text at a given position.  
     * delete(start, end): To delete a range of text.  
     * getText(): To retrieve the full document text (for debugging/initial render).  
5. **Bridge CLJS and WASM:**  
   * In editor-cljs, load the compiled core.wasm module.  
   * Write ClojureScript wrapper functions that handle the low-level interop of calling the exported WASM functions. This includes writing strings into the WASM linear memory and reading strings out of it.21  
   * **Checkpoint:** The editor should now use the WASM module for all text modifications. Typing in the editor should call the insert and delete functions in core-wasm.

---

### **Phase 3: Syntax Parsing with Tree-sitter**

**Goal:** Integrate Tree-sitter to provide robust, language-aware syntax highlighting.

1. **Set Up language-grammars Package:**  
   * Create the /packages/language-grammars directory.  
   * Add a subdirectory for a sample language, e.g., javascript/.  
   * Inside javascript/, add the grammar.js file from an existing Tree-sitter grammar repository and a minimal package.json to install tree-sitter-cli.5  
2. **Implement Grammar Compilation:**  
   * Create the scripts/build-grammars.sh script.  
   * This script should iterate through each subdirectory in language-grammars and run tree-sitter build \--wasm to compile the grammar into a .wasm file.25  
3. **Integrate web-tree-sitter:**  
   * Add web-tree-sitter as an NPM dependency to the editor-cljs package.  
   * In core.cljs, write code to initialize the parser and load the compiled javascript.wasm grammar file.  
4. **Implement Syntax Highlighting:**  
   * Create a CodeMirror 6 ViewPlugin that listens for document and viewport changes.  
   * On change, run the Tree-sitter parser over the visible text.  
   * Use a Tree-sitter query to identify different syntax nodes (keywords, strings, comments, etc.).  
   * Use the results of the query to create CodeMirror Decoration objects that apply the appropriate CSS classes for syntax highlighting.  
   * **Checkpoint:** The editor should now display correct syntax highlighting for the configured language as the user types.

---

### **Phase 4: Building the ClojureScript Shell & Emacs Features**

**Goal:** Leverage ClojureScript to build the editor's extensibility layer and core Emacs-like features.

1. **Command and Keymap System:**  
   * Implement a central command dispatcher in ClojureScript. This can be a map of command names (keywords) to functions.  
   * Use a CodeMirror Facet to define a keymap that translates key presses into command executions.  
   * Implement basic navigation and editing commands (e.g., forward-word, kill-line) that call the appropriate functions on the CodeMirror EditorView or our core-wasm module.  
2. **REPL-Driven Development Workflow:**  
   * Establish a robust workflow for interactive development. The shadow-cljs build process provides a live-reloading environment and a REPL connected to the browser.  
   * Document the process of connecting an editor (like Emacs with CIDER) to this REPL, allowing developers to evaluate code, inspect application state, and redefine functions in the running application.  
3. **UI Elements:**  
   * **Minibuffer:** Create a dedicated area for user input and feedback, controlled by ClojureScript.  
   * **Mode Line:** Implement a status bar that displays information like the current file name, cursor position, and active modes.

---

### **Phase 5: Backend Integration for Persistence**

**Goal:** Create a Clojure server to handle file persistence.

1. **Set Up backend-server Package:**  
   * Create the /packages/backend-server directory.  
   * Initialize a standard Clojure project using deps.edn.  
   * Add dependencies for a web server (e.g., Ring, Jetty) and data handling (e.g., a JSON library).  
2. **Implement a Simple API:**  
   * Create a web server that exposes a few initial API endpoints:  
     * POST /file: Receives file content from the front-end and saves it to disk.  
     * GET /file: Reads a file from disk and returns its content.  
3. **Front-End Communication:**  
   * In editor-cljs, implement functions to make fetch requests to these endpoints.  
   * Wire these functions to editor commands like save-buffer and find-file.  
   * **Checkpoint:** The user should be able to open a file from the server, edit it, and save it back.

---

### **Phase 6: Advanced Features**

**Goal:** Lay the groundwork for advanced IDE features.

1. **Language Server Protocol (LSP) Integration:**  
   * Plan the architecture for LSP support. The LSP client will live in editor-cljs.  
   * The client will communicate with language servers, which are separate processes, using JSON-RPC.  
   * Use a CodeMirror extension to display diagnostics (errors/warnings) from the server and another to provide completions.  
2. **Real-Time Collaboration:**  
   * Leverage the functional core's transaction-based updates. Each transaction is a delta that can be sent over the network.  
   * The backend-server will act as the central authority, receiving transactions from clients, transforming them to ensure consistency, and broadcasting them to other clients.  
   * The immutable nature of the core data structures makes this process much more manageable than with mutable state.3

## **6\. Development and Tooling**

* **Development Server:** The scripts/dev.sh script will be the primary entry point for development. It should start the shadow-cljs watcher, the Clojure backend server, and any other necessary processes.  
* **Testing:** The scripts/test.sh script will run all tests across all packages. Each package will have its own testing setup:  
  * core-wasm: Unit tests for the Rope and memory allocator.  
  * editor-cljs: Tests for commands and UI components.  
  * backend-server: Tests for API endpoints.  
* **Continuous Integration:** The .github/workflows/ci.yml file will define a GitHub Actions workflow to automatically run linting, tests, and a full build on every push and pull request, ensuring code quality and project stability.5

#### **Works cited**

1. CodeMirror System Guide, accessed September 25, 2025, [https://codemirror.net/docs/guide/](https://codemirror.net/docs/guide/)  
2. Betting on CodeMirror \- Replit Blog, accessed September 24, 2025, [https://blog.replit.com/codemirror](https://blog.replit.com/codemirror)  
3. Codemirror 6 Experiments \- Curran Kelleher \- Medium, accessed September 25, 2025, [https://currankelleher.medium.com/codemirror-6-experiments-a3930bf03781](https://currankelleher.medium.com/codemirror-6-experiments-a3930bf03781)  
4. CodeMirror 6 status update \- v6, accessed September 25, 2025, [https://discuss.codemirror.net/t/codemirror-6-status-update/2792](https://discuss.codemirror.net/t/codemirror-6-status-update/2792)  
5. Proposed Git Repository Structure for Lexicon  
6. Example: Writing a Language Package \- CodeMirror, accessed September 25, 2025, [https://codemirror.net/examples/lang-package/](https://codemirror.net/examples/lang-package/)  
7. Getting started | The AssemblyScript Book, accessed September 24, 2025, [https://www.assemblyscript.org/getting-started.html](https://www.assemblyscript.org/getting-started.html)  
8. Ropes. One of the most common data structures… | by Wyatt Saltzman | smucs \- Medium, accessed September 24, 2025, [https://medium.com/smucs/ropes-af2e2fcde39c](https://medium.com/smucs/ropes-af2e2fcde39c)  
9. Rope data structure : r/compsci \- Reddit, accessed September 25, 2025, [https://www.reddit.com/r/compsci/comments/1mstkav/rope\_data\_structure/](https://www.reddit.com/r/compsci/comments/1mstkav/rope_data_structure/)  
10. Using rope in text editors \- data structures \- Stack Overflow, accessed September 25, 2025, [https://stackoverflow.com/questions/46638217/using-rope-in-text-editors](https://stackoverflow.com/questions/46638217/using-rope-in-text-editors)  
11. How the rope data structure works when doing syntax highlighting, accessed September 25, 2025, [https://softwareengineering.stackexchange.com/questions/377165/how-the-rope-data-structure-works-when-doing-syntax-highlighting](https://softwareengineering.stackexchange.com/questions/377165/how-the-rope-data-structure-works-when-doing-syntax-highlighting)  
12. Rope (data structure) \- Wikipedia, accessed September 24, 2025, [https://en.wikipedia.org/wiki/Rope\_(data\_structure)](https://en.wikipedia.org/wiki/Rope_\(data_structure\))  
13. A complete novice writes Wasm by hand: Adding an Allocator | Bryan Burgers, accessed September 25, 2025, [https://burgers.io/complete-novice-wasm-allocator](https://burgers.io/complete-novice-wasm-allocator)  
14. Custom memory allocators – RasterGrid | Software Consultancy, accessed September 25, 2025, [https://www.rastergrid.com/blog/sw-eng/2021/03/custom-memory-allocators/](https://www.rastergrid.com/blog/sw-eng/2021/03/custom-memory-allocators/)  
15. Slab Allocator \- The Linux Kernel Archives, accessed September 25, 2025, [https://www.kernel.org/doc/gorman/html/understand/understand011.html](https://www.kernel.org/doc/gorman/html/understand/understand011.html)  
16. The Slab Allocator: An Object-Caching Kernel Memory Allocator \- People @EECS, accessed September 25, 2025, [https://people.eecs.berkeley.edu/\~kubitron/courses/cs194-24-S14/hand-outs/bonwick\_slab.pdf](https://people.eecs.berkeley.edu/~kubitron/courses/cs194-24-S14/hand-outs/bonwick_slab.pdf)  
17. Those that are writing a custom slab allocator. : r/C\_Programming \- Reddit, accessed September 25, 2025, [https://www.reddit.com/r/C\_Programming/comments/1m3qgt3/those\_that\_are\_writing\_a\_custom\_slab\_allocator/](https://www.reddit.com/r/C_Programming/comments/1m3qgt3/those_that_are_writing_a_custom_slab_allocator/)  
18. The SLAB Memory Allocator \- 3zanders.co.uk Alex Parker's Website, accessed September 25, 2025, [http://3zanders.co.uk/2018/02/24/the-slab-allocator/](http://3zanders.co.uk/2018/02/24/the-slab-allocator/)  
19. Slab allocation \- Wikipedia, accessed September 25, 2025, [https://en.wikipedia.org/wiki/Slab\_allocation](https://en.wikipedia.org/wiki/Slab_allocation)  
20. CodeMirror 6 scroll position testcase \- CodeSandbox, accessed September 25, 2025, [https://codesandbox.io/s/codemirror-6-scroll-position-testcase-7zcm4](https://codesandbox.io/s/codemirror-6-scroll-position-testcase-7zcm4)  
21. Getting Started with AssemblyScript \- SitePen, accessed September 24, 2025, [https://www.sitepen.com/blog/getting-started-with-assemblyscript](https://www.sitepen.com/blog/getting-started-with-assemblyscript)  
22. Memory – AssemblyScript – Introduction to Web Assembly, accessed September 25, 2025, [https://young.github.io/intro-to-web-assembly/assembly-script/memory/](https://young.github.io/intro-to-web-assembly/assembly-script/memory/)  
23. Memory Access with WebAssembly and Assembly Script \- Niles Tanner, accessed September 24, 2025, [https://www.blog.nilestanner.com/2018/11/18/memory-wasm-assembly-script/](https://www.blog.nilestanner.com/2018/11/18/memory-wasm-assembly-script/)  
24. Passing High-Level Data Types between AssemblyScript and JavaScript: Announcing as-bind \! | by Aaron Turner | Medium, accessed September 24, 2025, [https://medium.com/@torch2424/passing-high-level-data-types-between-assemblyscript-and-javascript-announcing-as-bind-9ea3daa4b4b9](https://medium.com/@torch2424/passing-high-level-data-types-between-assemblyscript-and-javascript-announcing-as-bind-9ea3daa4b4b9)  
25. I made a Buddy Allocator\! Is this useful? : r/embedded \- Reddit, accessed September 25, 2025, [https://www.reddit.com/r/embedded/comments/1co8417/i\_made\_a\_buddy\_allocator\_is\_this\_useful/](https://www.reddit.com/r/embedded/comments/1co8417/i_made_a_buddy_allocator_is_this_useful/)

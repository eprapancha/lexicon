# Lexicon Architecture

This document provides an overview of the Lexicon editor architecture and design decisions.

## Core Principles

- **Functional Core, Imperative Shell**: Pure, immutable data structures at the core with imperative browser interactions at the shell
- **Performance First**: WebAssembly for critical operations, efficient data structures
- **Extensibility**: Lisp-based extensibility system inspired by Emacs

## Technology Stack

- **Core**: AssemblyScript compiled to WebAssembly
- **Frontend**: ClojureScript with CodeMirror 6
- **Backend**: Clojure with Ring/Jetty
- **Parsing**: Tree-sitter grammars compiled to WebAssembly

## Package Structure

See the main README.md for detailed package organization and implementation phases.
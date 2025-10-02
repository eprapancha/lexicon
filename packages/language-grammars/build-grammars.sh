#!/bin/bash

# Build all Tree-sitter grammars to WebAssembly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Building Tree-sitter grammars..."

for grammar_dir in */; do
    if [ -d "$grammar_dir" ] && [ -f "${grammar_dir}grammar.js" ]; then
        echo "Building grammar: $grammar_dir"
        cd "$grammar_dir"
        
        # Install dependencies if package.json exists
        if [ -f "package.json" ]; then
            npm install
        fi
        
        # Build the grammar to WebAssembly
        if command -v tree-sitter &> /dev/null; then
            tree-sitter build --wasm
        elif command -v npx &> /dev/null; then
            npx tree-sitter build --wasm
        else
            echo "Warning: Neither tree-sitter nor npx found, skipping $grammar_dir"
            continue
        fi
        
        cd ..
    fi
done

echo "All grammars built successfully!"
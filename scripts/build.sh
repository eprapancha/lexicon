#!/bin/bash

# Build script for Lexicon

set -e

echo "Building Lexicon..."

# Build Rust WebAssembly core
echo "Building Rust WebAssembly core..."
cd packages/core-wasm
if command -v wasm-pack &> /dev/null; then
    wasm-pack build --target web
else
    echo "Warning: wasm-pack not found. Skipping WASM build."
    echo "Install wasm-pack: curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh"
fi

# Build Tree-sitter grammars
echo "Building Tree-sitter grammars..."
cd ../language-grammars
./build-grammars.sh

# Build ClojureScript frontend
echo "Building ClojureScript frontend..."
cd ../editor-cljs
npx shadow-cljs release app

cd ../..

echo "Build completed successfully!"
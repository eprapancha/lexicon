#!/bin/bash

# Build script for Lexicon

set -e

echo "Building Lexicon..."

# Build WebAssembly core
echo "Building WebAssembly core..."
cd packages/core-wasm
npm run asbuild

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
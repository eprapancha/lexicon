#!/bin/bash

# Test script for Lexicon

set -e

echo "Running Lexicon tests..."

# Test WebAssembly core
echo "Testing WebAssembly core..."
cd packages/core-wasm
npm test

# Test ClojureScript frontend
echo "Testing ClojureScript frontend..."
cd ../editor-cljs
npx shadow-cljs compile test

# Test Clojure backend
echo "Testing Clojure backend..."
cd ../backend-server
clj -M:test -m clojure.test

cd ../..

echo "All tests completed!"
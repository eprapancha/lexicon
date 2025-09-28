#!/bin/bash

# Integration test script for Lexicon editor
# Tests the complete WASM + ClojureScript integration

set -e

echo "🧪 Testing Lexicon WASM + ClojureScript Integration"
echo "=================================================="

# Build WASM module
echo "📦 Building WASM core module..."
cd packages/core-wasm
PATH=/home/nixos/.nix-profile/bin:$PATH npm run asbuild:debug
echo "✅ WASM module built successfully"

# Build ClojureScript application
echo "📦 Building ClojureScript application..."
cd ../editor-cljs
PATH=/home/nixos/.nix-profile/bin:$PATH npm install
PATH=/home/nixos/.nix-profile/bin:$PATH npx shadow-cljs compile app
echo "✅ ClojureScript application built successfully"

# Test WASM module accessibility
echo "🔍 Testing WASM module accessibility..."
if [ -f "../core-wasm/build/debug.wasm" ]; then
    echo "✅ WASM file exists at correct path"
    ls -la "../core-wasm/build/debug.wasm"
else
    echo "❌ WASM file not found"
    exit 1
fi

# Test ClojureScript build output
echo "🔍 Testing ClojureScript build output..."
if [ -f "resources/public/js/main.js" ]; then
    echo "✅ ClojureScript main.js exists"
    ls -la "resources/public/js/main.js"
else
    echo "❌ ClojureScript build output not found"
    exit 1
fi

# Check for critical functions in the built JavaScript
echo "🔍 Checking for critical functions in built output..."
if grep -q "lexicon.core" "resources/public/js/main.js"; then
    echo "✅ lexicon.core namespace found in build"
else
    echo "❌ lexicon.core namespace not found in build"
    exit 1
fi

echo ""
echo "🎉 Integration test completed successfully!"
echo ""
echo "To run the development environment:"
echo "1. In terminal 1: cd packages/editor-cljs && npm run dev"
echo "2. In terminal 2: cd packages/editor-cljs && npm run dev-server"  
echo "3. Open http://localhost:3001 in your browser"
echo ""
echo "The editor should load with:"
echo "- ✅ WASM module loaded and initialized"
echo "- ✅ Re-frame state management active"
echo "- ✅ Contenteditable with beforeinput handling"
echo "- ✅ IME composition support"
echo "- ✅ MutationObserver reconciliation"

cd ../..
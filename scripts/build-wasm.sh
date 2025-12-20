#!/bin/bash

# Build script for Lexicon WASM module
# Usage: ./scripts/build-wasm.sh

set -e

echo "🦀 Building Lexicon WASM module..."

# Ensure we're in the project root
cd "$(dirname "$0")/.."

# Set PATH to include nix profile tools
export PATH="/home/nixos/.nix-profile/bin:$PATH"

# Check if wasm-pack is available
if ! command -v wasm-pack &> /dev/null; then
    echo "❌ wasm-pack not found. Please install it:"
    echo "   nix-shell -p wasm-pack"
    echo "   or"
    echo "   curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh"
    exit 1
fi

# Navigate to WASM package directory
cd packages/lexicon-engine/wasm

echo "📦 Compiling Rust to WebAssembly..."
wasm-pack build --target web

echo "📁 Copying WASM files to ClojureScript resources..."
# Create target directory if it doesn't exist
mkdir -p ../../editor-cljs/resources/public/lexicon-engine/wasm/pkg/
# Copy built files to where ClojureScript expects them
cp pkg/* ../../editor-cljs/resources/public/lexicon-engine/wasm/pkg/

echo "✅ WASM build complete!"
echo ""
echo "📝 Built files:"
ls -la ../../editor-cljs/resources/public/lexicon-engine/wasm/pkg/
echo ""
echo "🔄 ClojureScript will automatically pick up the changes if running in watch mode."
#!/bin/bash

# Setup dependencies for editor-cljs
# Copies/links required static assets from node_modules

set -e

echo "Setting up editor-cljs dependencies..."

# Create js directory if it doesn't exist
mkdir -p resources/public/js

# Copy web-tree-sitter files
echo "Copying web-tree-sitter files..."
cp ../../node_modules/web-tree-sitter/tree-sitter.js resources/public/js/web-tree-sitter.js
cp ../../node_modules/web-tree-sitter/tree-sitter.wasm resources/public/js/tree-sitter.wasm

echo "✅ Dependencies setup complete"

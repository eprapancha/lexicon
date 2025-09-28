#!/bin/bash
set -e

echo "🚀 Building Lexicon Editor for GitHub Pages deployment..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if we're in the right directory
if [ ! -f "packages/editor-cljs/package.json" ]; then
    print_error "Please run this script from the project root directory"
    exit 1
fi

# Step 1: Build WASM module
print_status "Building WASM module..."
cd packages/lexicon-engine/wasm

if ! command -v wasm-pack &> /dev/null; then
    print_warning "wasm-pack not found. Installing..."
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

wasm-pack build --target web --out-dir pkg --release

# Copy WASM files to ClojureScript public directory
print_status "Copying WASM files to public directory..."
mkdir -p ../../editor-cljs/resources/public/lexicon-engine/wasm/
cp -r pkg ../../editor-cljs/resources/public/lexicon-engine/wasm/

cd ../../..

# Step 2: Install ClojureScript dependencies
print_status "Installing ClojureScript dependencies..."
cd packages/editor-cljs
npm install

# Step 3: Build ClojureScript
print_status "Building ClojureScript (checking for zero warnings)..."
BUILD_OUTPUT=$(npm run build 2>&1)
echo "$BUILD_OUTPUT"

if echo "$BUILD_OUTPUT" | grep -q "warnings"; then
    print_error "Build failed: Compilation warnings detected!"
    print_warning "Please fix all warnings before deployment."
    exit 1
else
    print_status "Clean compilation with 0 warnings!"
fi

cd ../..

# Step 4: Start local server for testing
print_status "Build complete! Starting local server..."
print_warning "Open http://localhost:8000 to test your build"
print_warning "Press Ctrl+C to stop the server"

cd packages/editor-cljs/resources/public
python3 -m http.server 8000 2>/dev/null || python -m http.server 8000
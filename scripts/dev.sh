#!/bin/bash

# Development script for Lexicon

set -e

echo "Starting Lexicon development environment..."

# Setup dependencies
echo "Setting up dependencies..."
cd packages/editor-cljs
./scripts/setup-deps.sh

# Start the ClojureScript development server
echo "Starting ClojureScript development server..."
npx shadow-cljs watch app &
CLJS_PID=$!

# Start the Clojure backend server
echo "Starting Clojure backend server..."
cd ../backend-server
clj -M -m lexicon.server &
BACKEND_PID=$!

cd ../..

echo "Development servers started!"
echo "Frontend: http://localhost:8080"
echo "Backend: http://localhost:3000"
echo ""
echo "Press Ctrl+C to stop all servers"

# Function to cleanup processes
cleanup() {
    echo "Stopping development servers..."
    kill $CLJS_PID 2>/dev/null || true
    kill $BACKEND_PID 2>/dev/null || true
    exit 0
}

# Trap Ctrl+C and cleanup
trap cleanup SIGINT

# Wait for processes
wait
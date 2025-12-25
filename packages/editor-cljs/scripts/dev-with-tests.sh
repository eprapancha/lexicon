#!/usr/bin/env bash

# Script to run both app and test builds concurrently
# This allows you to develop the app while tests watch for changes

set -e

echo "🚀 Starting Lexicon development environment with tests..."
echo ""
echo "This will start TWO shadow-cljs builds concurrently:"
echo "  • App build:  http://localhost:8080  (main application)"
echo "  • Test build: http://localhost:8021/test-index.html  (test suite)"
echo ""
echo "Both will watch for file changes and auto-reload."
echo ""
echo "Press Ctrl+C to stop both builds."
echo ""

# Function to cleanup background processes on exit
cleanup() {
    echo ""
    echo "🛑 Stopping all builds..."
    kill 0
}

trap cleanup EXIT

# Start test build in background
echo "📝 Starting test build on port 8021..."
npx shadow-cljs watch test &
TEST_PID=$!

# Give test build a moment to start
sleep 2

# Start app build in foreground (so Ctrl+C works naturally)
echo "🎨 Starting app build on port 8080..."
echo ""
npx shadow-cljs watch app

# If we get here, app build exited (shouldn't happen with watch)
wait $TEST_PID

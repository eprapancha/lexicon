#!/usr/bin/env bash

# Script to run Lexicon tests in Firefox headless mode
# Requires Firefox to be installed

set -e

echo "🧪 Running Lexicon test suite..."

# Check if Firefox is available
if ! command -v firefox &> /dev/null; then
    echo "❌ Firefox not found. Please install Firefox or run tests manually:"
    echo "   1. Start test server: npx shadow-cljs watch test"
    echo "   2. Open browser to: http://localhost:8021/test-index.html"
    exit 1
fi

# Check if test server is running
if ! curl -s http://localhost:8021/test-index.html > /dev/null 2>&1; then
    echo "❌ Test server not running on port 8021"
    echo "   Start it with: npx shadow-cljs watch test"
    exit 1
fi

echo "✅ Test server is running"
echo "🦊 Opening Firefox headless to run tests..."

# Run Firefox in headless mode
# This will open the test page and keep it open for 10 seconds
# You'll see console output in the terminal
timeout 10 firefox --headless --no-remote http://localhost:8021/test-index.html 2>&1 | \
    grep -E "(Testing|FAIL|PASS|Error|✅|❌)" || \
    echo "⚠️  Could not capture test output automatically"

echo ""
echo "📋 For detailed test results, open http://localhost:8021/test-index.html in a browser"
echo "   Tests will display with full UI including pass/fail status"

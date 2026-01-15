#!/usr/bin/env bash

# Fetch test results from the shadow-cljs test page
# This allows Claude to programmatically access test results

TEST_URL="http://localhost:8021/test"

# Fetch the page and extract test results from DOM
HTML=$(curl -s "$TEST_URL")

# Extract the test summary from the hidden div
SUMMARY=$(echo "$HTML" | grep -oP '<div id="test-summary"[^>]*>\K[^<]*' | head -1)

# Extract the JSON results
JSON=$(echo "$HTML" | grep -oP '<div id="test-results-json"[^>]*>\K[^<]*' | head -1)

if [ -n "$SUMMARY" ]; then
    echo "=== TEST SUMMARY ==="
    echo "$SUMMARY"
    echo ""
    echo "=== DETAILED RESULTS (JSON) ==="
    echo "$JSON" | python3 -m json.tool 2>/dev/null || echo "$JSON"
    exit 0
else
    echo "❌ Could not find test results. Make sure tests have run."
    exit 1
fi

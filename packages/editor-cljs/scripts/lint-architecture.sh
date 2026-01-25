#!/bin/bash
# Lint script to enforce architecture boundary
# Packages (files at src/lexicon/*.cljs, NOT in core/) must ONLY import lexicon.lisp
# They must NEVER import from lexicon.core.*

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/../src/lexicon"

echo "🔍 Checking architecture boundary..."

# Find all package files (files in src/lexicon/ but NOT in core/)
# Currently only dired.cljs, but this will catch future packages
PACKAGE_FILES=$(find "$SRC_DIR" -maxdepth 1 -name "*.cljs" ! -name "lisp.cljs" -type f)

VIOLATIONS=0

for file in $PACKAGE_FILES; do
    filename=$(basename "$file")

    # Skip lisp.cljs - it's the public API and can access internals
    if [ "$filename" = "lisp.cljs" ]; then
        continue
    fi

    # Check for imports from lexicon.core.*
    if grep -q "\[lexicon\.core\." "$file"; then
        echo "❌ VIOLATION: $filename imports from lexicon.core.*"
        echo "   Packages must ONLY import from lexicon.lisp"
        grep "\[lexicon\.core\." "$file"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi

    # Check for imports from re-frame (packages shouldn't use re-frame directly)
    if grep -q "\[re-frame\." "$file"; then
        echo "❌ VIOLATION: $filename imports from re-frame.*"
        echo "   Packages must ONLY import from lexicon.lisp"
        grep "\[re-frame\." "$file"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi

    # Check for imports from re-frame.db (direct state access)
    if grep -q "re-frame\.db" "$file"; then
        echo "❌ VIOLATION: $filename accesses re-frame.db directly"
        echo "   Packages must ONLY use lexicon.lisp functions"
        grep "re-frame\.db" "$file"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done

if [ $VIOLATIONS -eq 0 ]; then
    echo "✅ Architecture boundary check passed"
    exit 0
else
    echo ""
    echo "❌ Found $VIOLATIONS architecture violation(s)"
    echo ""
    echo "ARCHITECTURE RULE:"
    echo "  Packages (src/lexicon/*.cljs except lisp.cljs) must ONLY import:"
    echo "    [lexicon.lisp :as lisp]"
    echo ""
    echo "  They must NEVER import from:"
    echo "    - lexicon.core.*"
    echo "    - re-frame.*"
    echo "    - re-frame.db"
    echo ""
    echo "See: docs/ARCHITECTURE_BOUNDARY.md"
    exit 1
fi

# Test Directory Policy

**All editor tests MUST be implemented as E2E tests using Etaoin.**

Tests are located in: `/e2e_tests/lexicon/`

## Why E2E Tests Only?

ClojureScript unit tests in this directory have been deprecated because:

1. **API Boundary Enforcement**: ClojureScript tests frequently bypassed the Lisp API interface, reaching directly into re-frame events, subscriptions, and internal state. This made tests pass while the actual user-facing API was broken.

2. **False Confidence**: Tests that cheat by accessing internals provide false confidence. A passing test suite that doesn't exercise the real API boundaries is worse than no tests.

3. **E2E Tests the Real Thing**: Etaoin tests run against the actual browser application, testing the complete stack through the same interface users interact with.

## Running E2E Tests

```bash
# Run all E2E tests
bb test:e2e

# Run specific test file
bb test:e2e markers-test
bb test:e2e basic-editing-test
```

## Writing New Tests

1. Create test files in `/e2e_tests/lexicon/`
2. Use the `evalLisp` helper to test Lisp API functions
3. Test through the public API only - no internal access

See existing tests for examples.

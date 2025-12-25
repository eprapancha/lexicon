# Running Lexicon Automated Tests

## Overview

Lexicon uses **shadow-cljs browser-test** for automated testing. Tests are written in ClojureScript using `cljs.test` and run in a real browser environment to test re-frame, WASM, and React integration.

## Test Infrastructure

- **Test Build**: `:test` target in `shadow-cljs.edn`
- **Test Server**: Runs on http://localhost:8021
- **Test Runner**: `test/lexicon/test_runner.cljs`
- **Test Files**:
  - `test/lexicon/core_test.cljs` - Core editing functionality (15 tests)
  - `test/lexicon/window_test.cljs` - Window management (10 tests)
  - `test/lexicon/regression_test.cljs` - Regression tests (12 tests)

**Total**: 37 automated tests

## Running Tests

### Method 1: Manual Browser Testing (Recommended for Now)

1. **Start the test server**:
   ```bash
   cd /home/nixos/projects/lexicon/packages/editor-cljs
   npx shadow-cljs watch test
   ```

2. **Open test page in browser**:
   - URL: http://localhost:8021/index.html
   - Tests will run automatically and display results
   - Watch mode will recompile tests when you save changes

3. **View results**:
   - Test results appear in the browser UI
   - Console output shows detailed test execution
   - Failures are highlighted with error messages

### Method 2: Headless Browser Testing (Future)

Once `puppeteer` or `playwright` is installed, automated headless testing will be available:

```bash
# Install puppeteer (run once)
npm install --save-dev puppeteer

# Run tests headlessly
npm run test:headless
```

*(This requires fixing npm dependency issues first)*

## Test Watching

The `:test` build supports hot reload:
- Edit any test file
- Save changes
- Tests automatically recompile
- Refresh browser to see new results

## Writing Tests

### Test Structure

```clojure
(ns lexicon.my-feature-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]))

(defn reset-db! []
  (reset! rfdb/app-db (db/initial-db)))

(deftest test-my-feature
  (testing "My feature does X"
    (reset-db!)
    (rf/dispatch-sync [:initialize-db])

    ;; Test your feature
    (rf/dispatch-sync [:my-command])

    (is (= expected-value actual-value)
        "Description of what should happen")))
```

### Best Practices

1. **Reset state**: Call `reset-db!` before each test
2. **Use `dispatch-sync`**: For deterministic test execution
3. **Test one thing**: Each test should verify one behavior
4. **Clear descriptions**: Explain what the test verifies
5. **Regression tests**: When fixing a bug, add a test that would have caught it

### Adding New Tests

1. Add test to existing file or create new test file
2. If new file, require it in `test/lexicon/test_runner.cljs`:
   ```clojure
   (ns lexicon.test-runner
     (:require [lexicon.my-feature-test]))  ;; Add this

   (defn run-all-tests []
     (run-tests 'lexicon.my-feature-test))  ;; Add this
   ```
3. Save and let shadow-cljs recompile
4. Refresh browser to see new tests

## Test Output Examples

### Passing Tests
```
Testing lexicon.core-test

Ran 15 tests containing 32 assertions.
0 failures, 0 errors.
```

### Failing Tests
```
Testing lexicon.core-test

FAIL in (test-insert-text) (...:42)
Inserting text at point
expected: "Hello, World!"
  actual: "Hello World!"

Ran 15 tests containing 32 assertions.
1 failures, 0 errors.
```

## Troubleshooting

### Tests Don't Run

**Problem**: Browser shows blank page or errors

**Solutions**:
1. Check WASM files exist: `ls resources/public/lexicon-engine/wasm/pkg/`
2. Rebuild WASM: `cd ../lexicon-engine/wasm && wasm-pack build --target web`
3. Clear browser cache and refresh
4. Check console for JavaScript errors

### WASM Not Loading

**Problem**: Tests fail with "WASM module not found"

**Solutions**:
1. Verify WASM path in `test_runner.cljs` matches actual file location
2. Check browser network tab for 404 errors
3. Ensure test server is serving files from correct root directory

### Tests Pass But Features Broken

**Problem**: Tests pass but manual testing shows bugs

**Solutions**:
1. Tests may not cover all edge cases
2. Add regression tests for the bug
3. Check if test setup matches real usage
4. Verify WASM initialization in tests matches app initialization

### Hot Reload Not Working

**Problem**: Changes don't appear after saving

**Solutions**:
1. Check shadow-cljs terminal for compilation errors
2. Hard refresh browser (Ctrl+Shift+R)
3. Restart `shadow-cljs watch test`

## CI/CD Integration (Future)

Once headless testing works, add to CI pipeline:

```yaml
# .github/workflows/test.yml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
      - name: Install dependencies
        run: npm ci
      - name: Run tests
        run: npm run test:headless
```

## Performance

- **Initial compile**: 3-5 seconds
- **Hot reload**: 0.5-2 seconds
- **Test execution**: < 1 second (37 tests)

## Next Steps

1. **Set up Puppeteer**: Enable headless automated testing
2. **Add coverage**: Expand test coverage for all features
3. **CI Integration**: Run tests on every commit
4. **Performance tests**: Add benchmarks for critical paths
5. **E2E tests**: Test complete user workflows

---

**Test Status**: ✅ 37 automated tests passing
**Last Updated**: 2025-12-25

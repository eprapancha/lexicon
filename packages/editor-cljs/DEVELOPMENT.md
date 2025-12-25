# Lexicon Development Guide

## Development Workflow

Lexicon uses **shadow-cljs** for ClojureScript compilation with two separate builds:

### The Two Builds

| Build | Port | Purpose | Command |
|-------|------|---------|---------|
| `:app` | 8080 | Main application | `npx shadow-cljs watch app` |
| `:test` | 8021 | Test suite | `npx shadow-cljs watch test` |

**Both builds can run simultaneously** in separate terminals or together.

---

## Quick Start

### Option 1: Development with Live Tests (Recommended)

**Run both builds together** for the best development experience:

```bash
cd /home/nixos/projects/lexicon/packages/editor-cljs
./scripts/dev-with-tests.sh
```

This opens:
- **App**: http://localhost:8080 - Your main development environment
- **Tests**: http://localhost:8021/test-index.html - Live test results

**Workflow**:
1. Write code in your editor
2. Watch app reload at http://localhost:8080
3. Watch tests re-run at http://localhost:8021/test-index.html
4. Fix any test failures immediately
5. Both builds auto-reload on file save

**Stop both**: Press Ctrl+C (kills both builds cleanly)

---

### Option 2: App Development Only

**Just develop the app** without running tests:

```bash
cd /home/nixos/projects/lexicon/packages/editor-cljs
npx shadow-cljs watch app
```

Open http://localhost:8080

---

### Option 3: Run Tests Separately

**While app is running**, open a new terminal:

```bash
# Terminal 1: Keep this running
npx shadow-cljs watch app

# Terminal 2: New terminal
cd /home/nixos/projects/lexicon/packages/editor-cljs
npx shadow-cljs watch test
```

Open http://localhost:8021/test-index.html

---

## Testing Workflow

### Before Starting a New Feature

1. **Start both builds**: `./scripts/dev-with-tests.sh`
2. **Verify tests pass**: Check http://localhost:8021/test-index.html shows all green
3. **Start coding**: Tests will re-run as you save files

### While Developing

1. **Write code** in `src/lexicon/...`
2. **Save file** → Both app and tests auto-reload
3. **Check test results** in browser (should stay open)
4. **Fix failures immediately** - don't accumulate broken tests

### After Completing a Feature

1. **Verify all tests pass** at http://localhost:8021/test-index.html
2. **Write new tests** for the feature (if needed)
3. **Commit changes** only when tests are green

### Writing New Tests

1. **Create or edit test file** in `test/lexicon/`
2. **Tests auto-reload** when you save
3. **Check browser** to see new test results
4. **See [test/README.md](./test/README.md)** for testing guide

---

## Build Commands Reference

```bash
# Start app only
npx shadow-cljs watch app

# Start tests only
npx shadow-cljs watch test

# Start both (recommended)
./scripts/dev-with-tests.sh

# Compile app for production
npx shadow-cljs release app

# Compile tests once (no watch)
npx shadow-cljs compile test

# REPL connected to app
npx shadow-cljs cljs-repl app

# REPL connected to tests
npx shadow-cljs cljs-repl test

# Stop shadow-cljs server
npx shadow-cljs stop
```

---

## File Structure

```
packages/editor-cljs/
├── src/lexicon/          # Application code
│   ├── core.cljs         # Main entry point
│   ├── events.cljs       # re-frame events
│   ├── subs.cljs         # re-frame subscriptions
│   └── ...
│
├── test/lexicon/         # Test code
│   ├── core_test.cljs    # Core functionality tests
│   ├── window_test.cljs  # Window management tests
│   ├── regression_test.cljs  # Regression tests
│   └── test_runner.cljs  # Test namespace loader
│
├── resources/public/     # Compiled output & static assets
│   ├── index.html        # App entry point
│   ├── test-index.html   # Test entry point
│   ├── js/               # Compiled app code
│   └── test/js/          # Compiled test code
│
└── scripts/
    ├── dev-with-tests.sh # Run app + tests together
    └── run-tests.sh      # Run tests in Firefox headless
```

---

## Tips & Tricks

### Keep Tests Open While Developing

1. **Use two browser windows**:
   - Left window: http://localhost:8080 (your app)
   - Right window: http://localhost:8021/test-index.html (tests)
2. **Tile them side-by-side** to see both at once
3. **Tests update automatically** when you save files

### REPL-Driven Development

```bash
# Start REPL connected to running app
npx shadow-cljs cljs-repl app

# Now you can evaluate code in the running app
(in-ns 'lexicon.core)
(def my-test-data {...})
```

### Debugging Test Failures

1. **Open browser DevTools** on test page
2. **Check Console** for detailed assertion failures
3. **Set breakpoints** in `test/lexicon/*_test.cljs`
4. **Use `println`** in tests to inspect values

### Faster Compilation

```bash
# Clear compiled cache if builds get slow
npx shadow-cljs stop
rm -rf .shadow-cljs
npx shadow-cljs watch app
```

---

## Common Issues

### Port Already in Use

```
Error: Port 8080 already in use
```

**Solution**: Stop existing shadow-cljs or kill process:
```bash
npx shadow-cljs stop
# or
lsof -ti:8080 | xargs kill -9
```

### Tests Not Updating

**Solution**: Hard refresh browser (Ctrl+Shift+R)

### WASM Not Loading

**Solution**: Rebuild WASM module:
```bash
cd ../lexicon-engine/wasm
wasm-pack build --target web
```

---

## Next Steps

- **Writing Tests**: See [test/README.md](./test/README.md)
- **Running Tests**: See [test/RUNNING_TESTS.md](./test/RUNNING_TESTS.md)
- **Architecture**: See [../../docs/architecture.md](../../docs/architecture.md)
- **Roadmap**: See [../../docs/ROADMAP.md](../../docs/ROADMAP.md)

---

**Happy coding! 🚀**

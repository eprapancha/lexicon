# Chrome Extension Manual Testing Workflow

**Purpose:** Use the Claude Chrome Extension as a "browser agent" to perform manual testing of the running Lexicon app, bridging the gap between automated E2E tests and actual browser behavior.

---

## Why This Exists

E2E tests (Etaoin/WebDriver) don't always catch real-world issues:
- Tests pass but the app is visually broken
- DOM focus, timing, and rendering issues invisible to WebDriver
- Console errors that don't cause test assertions to fail
- Keyboard event routing bugs specific to real browser behavior

The Chrome Extension can see the actual rendered page, interact with it like a user, read the DevTools console, and report back structured findings.

---

## Workflow

```
1. Make code changes (CLI Claude session)
2. Wait for shadow-cljs hot reload (or hard refresh)
3. Generate test prompt (use /chrome-test skill)
4. Paste prompt into Chrome Extension
5. Extension performs tests, reports results
6. Share report back with CLI Claude session
7. CLI Claude diagnoses and fixes issues
8. Repeat from step 2
```

---

## Best Practices for Test Prompts

### Structure

1. **Context** - Tell the extension what it's testing and where
2. **Setup** - Any preconditions (refresh, wait for load, clear console)
3. **Test steps** - Specific actions with expected outcomes
4. **Report format** - Exact structure for copy-paste back to CLI

### Writing Effective Tests

- **Be specific about key combos**: "Press Ctrl+X, then press b" not "Press C-x b"
- **State expected outcomes**: "The echo area should show 'hi'" not "Check eval works"
- **Include console checks**: Always ask for errors before AND after tests
- **Report format must be copy-pasteable**: Use code blocks in the prompt
- **Keep tests focused**: 4-6 tests per prompt, not 20
- **Include failure descriptions**: "Or did nothing happen / error occur?"

### Key Mappings for Prompts

The extension operates in a real browser, so use browser key names:

| Emacs | Browser | Prompt wording |
|-------|---------|----------------|
| C-x | Ctrl+X | "Press Ctrl+X" |
| M-x | Alt+X | "Press Alt+X" |
| M-: | Alt+Shift+; | "Press Alt+Shift+; (produces M-:)" |
| RET | Enter | "Press Enter" |
| TAB | Tab | "Press Tab" |
| C-g | Ctrl+G | "Press Ctrl+G" |
| SPC | Space | "Press Space" |

### Report Format Template

Always request structured output:
```
TEST REPORT
===========
Test name: [PASS/FAIL - brief description]
```

This makes it easy to parse results in the CLI session.

---

## Common Test Scenarios

### After fixing minibuffer issues
- Test C-x b → type → Enter confirms
- Test M-: → eval → result in echo area
- Test M-x → command name → executes
- Check console for `:fx` or protocol errors

### After fixing keybinding issues
- Test that all letters type correctly in minibuffer
- Test that modifier combos work (C-f, C-b, M-f, M-b)
- Test multi-key sequences (C-x C-f, C-x C-s)

### After fixing rendering issues
- Check mode line updates correctly
- Check cursor position indicator
- Check echo area messages appear/disappear

### After fixing package/eval issues
- Test M-: with various expressions
- Check SCI eval results display correctly
- Test install-package with lexpa server running

---

## Limitations

- Extension is a separate Claude instance -- no shared context with CLI
- Results must be manually copy-pasted between sessions
- Extension can't run shell commands or modify files
- Some keyboard shortcuts may be intercepted by Chrome itself (Ctrl+W, Ctrl+T, Ctrl+N)
- Extension may not have access to all DevTools features

---

## Integration with E2E Tests

The Chrome Extension testing complements (does NOT replace) automated E2E tests:

| Aspect | E2E Tests | Chrome Extension |
|--------|-----------|-----------------|
| Automation | Fully automated | Manual prompt cycle |
| Speed | Fast batch runs | Slow interactive |
| DOM focus | Simulated | Real browser focus |
| Console errors | Not checked | Directly visible |
| Visual rendering | Not verified | Directly visible |
| Regression | CI-ready | Ad-hoc debugging |

**Rule:** If the Chrome Extension finds a bug, write an E2E test that catches it before fixing it (TDD).

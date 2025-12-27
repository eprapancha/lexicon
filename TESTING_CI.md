# Testing CI Before Pushing to GitHub

Stop wasting time on failed GitHub Actions! Test locally first.

## Quick Start

### Method 1: Fast Local Test (Recommended for iteration)

```bash
bb ci-test
```

**What it does:**
- Checks all dependencies
- Runs full build
- Runs tests
- Takes ~2-5 minutes

**Pros:**
- ✅ Fast
- ✅ No Docker required
- ✅ Uses your local environment

**Cons:**
- ⚠️ Not 100% identical to GitHub (but close enough)

---

### Method 2: Full GitHub Actions Simulation (Recommended before push)

```bash
# One-time setup (installs act)
bb install-act

# Test the CI workflow (exactly like GitHub)
bb act-ci
```

**What it does:**
- Runs the actual `.github/workflows/ci.yml` in Docker
- Identical to GitHub Actions environment
- Takes ~10-20 minutes (first run downloads images)

**Pros:**
- ✅ 100% identical to GitHub
- ✅ Tests the actual workflow file
- ✅ Catches environment-specific issues

**Cons:**
- ⚠️ Slower
- ⚠️ Requires Docker

---

## Recommended Workflow

```bash
# 1. Quick test during development
bb ci-test

# 2. Make changes...
# 3. Quick test again
bb ci-test

# 4. When ready to push, do final verification
bb act-ci

# 5. If act-ci passes, push to GitHub
git push
```

This workflow ensures:
- Fast iteration with `bb ci-test`
- Final verification with `bb act-ci`
- No more failed CI on GitHub!

---

## Common Issues

### "Docker not found" when running act-ci

**Solution:** Install Docker first:
```bash
# Ubuntu/Debian
sudo apt-get install docker.io

# macOS
brew install docker

# Or download from docker.com
```

### act-ci takes forever on first run

**Normal!** The first run downloads Docker images (~2-3 GB). Subsequent runs are much faster.

### act-ci fails but GitHub Actions succeeds

This is rare. Usually means:
- Different secrets/environment variables
- GitHub-specific features (like Pages deployment)

For build/test issues, act is extremely accurate.

---

## Other Useful Commands

```bash
# List all available workflows
bb act-list

# Run just the build step (faster than full CI)
bb build

# Clean everything and rebuild
bb clean && bb build
```

---

## Why This Matters

**Before:**
```
Edit code → Push → Wait 5 min → CI fails
→ Fix → Push → Wait 5 min → CI fails again
→ Fix → Push → Wait 5 min → Finally passes!
Total: 15+ minutes, 3 failed commits
```

**After:**
```
Edit code → bb ci-test (2 min) → Fix → bb ci-test (2 min) → Fix
→ bb act-ci (10 min) → Passes!
→ Push → CI passes first time!
Total: 14 minutes, 0 failed commits on GitHub
```

You save time AND have a cleaner git history!

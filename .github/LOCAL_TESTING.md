# Testing GitHub Actions Locally

## Recommended: Use Babashka (Fastest)

The easiest way to test CI locally is with the built-in task:

```bash
bb ci-test
```

This runs the exact same steps as GitHub Actions:
- Checks all required dependencies
- Installs packages
- Builds the project
- Runs tests

**Advantages:**
- ✅ Fast (no Docker overhead)
- ✅ Uses your local environment
- ✅ No additional tools needed
- ✅ Shows exactly what will happen on GitHub

## Alternative: Use act (Full Simulation)

You can also test GitHub Actions locally using [act](https://github.com/nektos/act), which runs your workflows in Docker containers.

## Install act

### NixOS
```bash
nix-shell -p act
```

### Or use the installer
```bash
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

## Basic Usage

### List available workflows
```bash
act -l
```

### Run CI workflow
```bash
# Dry run (shows what would happen)
act -n

# Run the CI workflow
act pull_request

# Run a specific job
act -j test
```

### Run Deploy workflow
```bash
# Run on push event
act push

# Run specific workflow file
act -W .github/workflows/deploy.yml
```

## Common Options

```bash
# Use a specific Docker image (smaller)
act -P ubuntu-latest=catthehacker/ubuntu:act-latest

# Pass secrets
act -s GITHUB_TOKEN=your_token

# Verbose output
act -v

# Don't pull Docker images
act --pull=false

# Use a specific event
act -e .github/workflows/test-event.json
```

## Limitations

Some things won't work exactly like GitHub:
- `deploy-pages` action won't actually deploy
- Some GitHub-specific contexts might be different
- Caching may behave differently

## Recommended Workflow

1. **Test locally with act first:**
   ```bash
   act pull_request -j test
   ```

2. **If it works, push to a feature branch:**
   ```bash
   git checkout -b test-ci
   git add .
   git commit -m "Test CI workflow"
   git push -u origin test-ci
   ```

3. **Create a draft PR to test on GitHub**

## Quick CI Test

For our project, test the CI workflow:

```bash
# Install act
nix-shell -p act

# List jobs
act -l

# Run CI test job
act pull_request -j test
```

## Alternative: Use GitHub CLI

You can also monitor workflows without pushing:

```bash
# Install gh
nix-shell -p gh

# View workflow runs
gh run list

# Watch a specific run
gh run watch

# View logs
gh run view --log
```

## Minimal Test Before Push

If act is too heavy, do a minimal local test:

```bash
# Test the build commands manually
bb install-cljs
bb build

# If successful, the CI should work too
```

# GitHub Pages Deployment Setup

This repository is configured for automatic deployment to GitHub Pages using GitHub Actions.

## 🚀 Deployment Process

Every push to the `main` branch triggers:

1. **WASM Build**: Compiles Rust to WebAssembly
2. **ClojureScript Build**: Compiles CLJS to optimized JavaScript  
3. **Zero Warnings Check**: Ensures clean compilation
4. **GitHub Pages Deploy**: Publishes to your GitHub Pages site

## 📋 Setup Steps

### 1. Enable GitHub Pages
1. Go to your repository **Settings** → **Pages**
2. Set **Source** to "GitHub Actions"
3. Save the settings

### 2. Repository Permissions
The workflow is already configured with the correct permissions:
- `contents: read` - To checkout code
- `pages: write` - To deploy to Pages
- `id-token: write` - For GitHub Pages authentication

### 3. Your Site URL
After the first successful deployment, your Lexicon editor will be available at:
```
https://YOUR_USERNAME.github.io/YOUR_REPOSITORY_NAME
```

## 🔧 Development

### Local Development
```bash
# In packages/editor-cljs/
npm install
npm run dev
```
Visit http://localhost:8080

### Production Build (Local)
```bash
# Build WASM
cd packages/lexicon-engine/wasm
wasm-pack build --target web --out-dir pkg --release

# Build ClojureScript  
cd ../../editor-cljs
npm run build
```

## ✅ What's Automated

- ✅ **Rust/WASM compilation** with caching
- ✅ **ClojureScript compilation** with zero warnings enforcement
- ✅ **Dependency caching** for faster builds
- ✅ **Pull request testing** to catch issues early
- ✅ **Automatic deployment** on main branch pushes

## 🎯 Zero Warnings Policy

The CI enforces our **zero warnings** policy:
- Any ClojureScript compilation warnings will fail the build
- This ensures clean, professional code quality
- All type hints and requires must be properly specified

## 📊 Build Status

Check the **Actions** tab in your GitHub repository to monitor build status and deployments.
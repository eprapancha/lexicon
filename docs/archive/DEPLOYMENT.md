# Lexicon Editor - Deployment Guide

## 🚧 Current Status: "Men At Work" Project

Lexicon is currently a **static web application** perfect for GitHub Pages deployment. 

## 🚀 GitHub Pages Deployment (Primary)

### Automatic Deployment
Push to `main` branch → GitHub Actions automatically builds and deploys

The workflow:
- ✅ Builds Rust WASM modules
- ✅ Compiles Tree-sitter grammars  
- ✅ Bundles ClojureScript application
- ✅ Deploys to `https://yourusername.github.io/lexicon`

### Option 2: Manual Build & Deploy

```bash
cd packages/editor-cljs
./deploy.sh
python -m http.server 8000 --directory resources/public
```

### Option 3: Static Site Hosting

**Netlify** (Drag & Drop):
1. Build locally: `cd packages/editor-cljs && ./deploy.sh`
2. Drag `resources/public/` folder to netlify.com
3. Done! ✅

**Vercel**:
```bash
npm install -g vercel
cd packages/editor-cljs/resources/public
vercel --prod
```

**Any Web Server**:
```bash
# Copy built files to your server
scp -r resources/public/* user@server:/var/www/html/
```

## 🛠️ Build Requirements

- **Node.js 18+** (for ClojureScript and Tree-sitter)
- **Java 11+** (for ClojureScript compilation) 
- **Rust + wasm-pack** (for WASM modules)
- **tree-sitter CLI** (for grammar compilation)

## 📦 Production Build Includes

- ClojureScript application bundle
- Rust WASM text engine
- Tree-sitter library and grammars
- Syntax parsing web worker
- All static assets optimized

## 🌐 Hosting Requirements

**MIME Types**: Ensure `.wasm` files served as `application/wasm`

**Security Headers**: 
```
Content-Security-Policy: default-src 'self' 'unsafe-inline' 'unsafe-eval'; worker-src 'self' blob:
```

**HTTPS**: Required for Web Workers and WASM in some browsers

## 🎯 Production Checklist

- [ ] GitHub Actions workflow runs successfully
- [ ] All WASM files load correctly  
- [ ] Syntax highlighting works
- [ ] M-x commands function properly
- [ ] File operations work
- [ ] No console errors
- [ ] HTTPS enabled
- [ ] Proper MIME types configured
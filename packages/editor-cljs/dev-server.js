// Simple development server to serve both ClojureScript and WASM files
import { createServer } from 'http';
import { readFileSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const server = createServer((req, res) => {
  const url = req.url;
  
  // CORS headers for development
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
  
  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }
  
  try {
    if (url === '/') {
      // Serve index.html
      const htmlPath = join(__dirname, 'resources/public/index.html');
      if (existsSync(htmlPath)) {
        const html = readFileSync(htmlPath, 'utf8');
        res.setHeader('Content-Type', 'text/html');
        res.writeHead(200);
        res.end(html);
      } else {
        res.writeHead(404);
        res.end('index.html not found');
      }
    } else if (url.startsWith('/js/')) {
      // Serve JavaScript files
      const jsPath = join(__dirname, 'resources/public', url);
      if (existsSync(jsPath)) {
        const js = readFileSync(jsPath, 'utf8');
        res.setHeader('Content-Type', 'application/javascript');
        res.writeHead(200);
        res.end(js);
      } else {
        res.writeHead(404);
        res.end(`JS file not found: ${url}`);
      }
    } else if (url === '/core-wasm/build/debug.wasm') {
      // Serve WASM file from the core-wasm package
      const wasmPath = join(__dirname, '../core-wasm/build/debug.wasm');
      if (existsSync(wasmPath)) {
        const wasm = readFileSync(wasmPath);
        res.setHeader('Content-Type', 'application/wasm');
        res.writeHead(200);
        res.end(wasm);
      } else {
        console.log('WASM file not found at:', wasmPath);
        res.writeHead(404);
        res.end('WASM file not found. Please run: npm run asbuild:debug in core-wasm package');
      }
    } else {
      res.writeHead(404);
      res.end('Not found');
    }
  } catch (error) {
    console.error('Server error:', error);
    res.writeHead(500);
    res.end('Internal server error');
  }
});

const PORT = 3001;
server.listen(PORT, () => {
  console.log(`Development server running at http://localhost:${PORT}`);
  console.log('Make sure to build the WASM module first: cd ../core-wasm && npm run asbuild:debug');
});
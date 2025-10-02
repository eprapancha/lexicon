# Lexicon Bridge

A WebSocket bridge server that enables the browser-based Lexicon editor to communicate with local system tools like Language Server Protocol (LSP) servers.

## Features

- WebSocket server for browser-to-system communication
- LSP server process management
- Automatic process spawning and lifecycle management
- Support for multiple languages: JavaScript, TypeScript, Python, Rust, Clojure

## Prerequisites

Install the required LSP servers for the languages you want to use:

- **JavaScript/TypeScript**: `npm install -g typescript-language-server`
- **Python**: `pip install python-lsp-server`
- **Rust**: Install `rust-analyzer` from your package manager
- **Clojure**: Install `clojure-lsp`

## Usage

1. Install dependencies:
   ```bash
   npm install
   ```

2. Start the bridge server:
   ```bash
   npm start
   ```

The server will listen on `localhost:30303` by default.

## How it works

1. Browser connects to WebSocket server
2. Editor sends LSP start requests for specific languages
3. Bridge spawns appropriate LSP server processes
4. All LSP JSON-RPC messages are proxied between browser and LSP servers
5. Multiple clients can connect to the same language servers

## Message Format

Messages sent over WebSocket follow this format:

```json
{
  "type": "lsp/start|lsp/message|lsp/stop",
  "language": "javascript|typescript|python|rust|clojure",
  "data": { ... }
}
```

## Development

Run with debugging:
```bash
npm run dev
```
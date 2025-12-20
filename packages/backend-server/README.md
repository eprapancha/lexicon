# backend-server

## Status: **Not Currently Used**

This package is a **placeholder** for future cloud IDE features planned in the **L1.8: "The Collaborative Cloud IDE"** roadmap phase.

## Current State

The backend-server is currently a minimal Clojure/Ring HTTP server stub with:
- Basic health check endpoint (`GET /health`)
- Placeholder file endpoints (not implemented)
- Runs on port 3000

**You do NOT need to run this server for local development.**

## What You Should Use Instead

For **local development** with LSP support, use the **`lexicon-bridge`** package instead:

```bash
cd packages/lexicon-bridge
node index.js
```

The lexicon-bridge is a Node.js WebSocket server that:
- Provides LSP (Language Server Protocol) integration
- Bridges the browser to native language servers (TypeScript, Rust, Python, etc.)
- Uses secure ticket-based WebSocket authentication
- Is **actively used** in the current implementation

## Future Purpose (L1.8 Roadmap)

When fully implemented, this backend-server will provide:

### 1. **Backend Orchestration Service**
- Manage containerized development workspaces
- Provision and teardown cloud-based development environments
- Handle workspace lifecycle management

### 2. **"Workspace as Code"**
- Support for `devcontainer.json` configuration
- Declarative workspace definitions
- Reproducible development environments in the cloud

### 3. **Real-time Collaborative Editing**
- Multi-user editing with conflict resolution
- Operational Transform (OT) or CRDT-based synchronization
- Presence awareness and cursor sharing

### 4. **Cloud-Native IDE Backend**
- File system operations for cloud workspaces
- Authentication and authorization
- Project management and persistence

## Running the Server (Optional)

If you want to run the placeholder server:

```bash
cd packages/backend-server
clj -M -m lexicon.server
```

Or with a custom port:

```bash
clj -M -m lexicon.server 3000
```

The server will start on the specified port (default: 3000).

## Architecture Comparison

| Feature | lexicon-bridge (Node.js) | backend-server (Clojure) |
|---------|-------------------------|--------------------------|
| **Status** | ✅ Active | ⏳ Future placeholder |
| **Use Case** | Local LSP integration | Cloud IDE orchestration |
| **Protocol** | WebSocket + HTTP | HTTP/REST (future: WebSocket) |
| **Port** | 30303 (WS), 30304 (HTTP) | 3000 |
| **Required?** | Yes (for LSP features) | No (not implemented) |

## Development

The package uses Clojure CLI tools with the following structure:

```
backend-server/
├── deps.edn          # Dependencies and build config
├── src/
│   └── lexicon/
│       └── server.clj # Main server implementation
├── test/             # Test files (empty)
└── resources/        # Static resources
```

### Dependencies

- `org.clojure/clojure` - Clojure runtime
- `ring/ring-core` - HTTP server
- `compojure` - Routing
- `cheshire` - JSON encoding/decoding

## Contributing

If you're interested in implementing the L1.8 cloud IDE features, see:
- Main project roadmap in `/README.md`
- Architecture docs in `/docs/` (when available)
- Related issues tagged with `L1.8` or `cloud-ide`

---

**TL;DR:** Skip this package for now. Use `lexicon-bridge` instead for local development.

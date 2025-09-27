# Lexicon Core WASM - Rust Implementation

This package contains the new Rust-based WebAssembly kernel for the Lexicon editor, implementing a high-performance Piece Tree data structure.

## Architecture

### Piece Tree Data Structure

The implementation follows the state-of-the-art "VS Code" model with the following components:

1. **Immutable Buffers**:
   - `OriginalBuffer`: Contains the initial file content (immutable)
   - `AddBuffer`: Append-only buffer for all new insertions

2. **Piece Descriptor**:
   - Small object containing a pointer to one of the buffers
   - Start offset and length within the buffer
   - Buffer type (Original or Add)

3. **Red-Black Tree**:
   - Self-balancing binary tree with Piece descriptors as leaf nodes
   - Ensures O(log k) operations where k is the number of pieces

4. **Metadata Caching**:
   - Each internal node caches aggregate metadata about its subtrees
   - Includes `char_count` and `line_break_count` for left child
   - Enables O(log k) navigation to any character or line number

### Memory Management

This implementation targets **WasmGC** for memory management:
- No custom memory allocators (Slab, Buddy, etc.)
- Uses standard Rust data structures (Box, Rc, Vec)
- Relies on browser's native, highly-optimized garbage collector
- Dramatically simplifies code and reduces memory corruption risks

## API

The public API is exposed via `wasm-bindgen` and provides:

- `PieceTree::new(initial_text)` - Create new piece tree
- `insert(position, text)` - Insert text at position
- `get_text()` - Retrieve full document text
- `length()` - Get total character count
- `line_count()` - Get total line count
- `get_line(line_number)` - Get specific line text

## Building

Requires the Rust toolchain with WebAssembly targets:

```bash
# Install wasm-pack if not already installed
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Add WebAssembly target
rustup target add wasm32-unknown-unknown

# Build the WebAssembly module
wasm-pack build --target web
```

This will generate the `pkg/` directory containing:
- `core_wasm.js` - JavaScript bindings
- `core_wasm_bg.wasm` - WebAssembly binary
- `core_wasm.d.ts` - TypeScript definitions

## Testing

Open `test.html` in a web browser to run basic functionality tests. The test file demonstrates:
- Basic initialization
- Text insertion
- Text retrieval  
- Line operations

## Integration

### ClojureScript Compatibility

The WASM module provides two API levels for maximum compatibility:

1. **Modern API** (`EditorState` class):
   - Immutable, functional design with `apply_transaction(json)`
   - Uses CodeMirror-style transaction format: `{"changes": [{"from": N, "to": M, "insert": "text"}]}`
   - Returns `Result<EditorState, JsValue>` for robust error handling

2. **Legacy Compatibility API** (`WasmEditorCore` class):
   - Designed for seamless integration with existing ClojureScript code
   - Mutable interface matching original AssemblyScript expectations
   - Methods: `init()`, `applyTransaction()`, `getLastResult()`, `getLastErrorMessage()`
   - Transaction format: `{"type": 0, "position": N, "text": "..."}`  (insert)
   - Transaction format: `{"type": 1, "position": N, "length": M}`     (delete)
   - Transaction format: `{"type": 2, "position": N, "length": M, "text": "..."}`  (replace)
   - Direct mutation methods: `insertText()`, `deleteText()`, `getText()`, etc.
   - Error handling via return codes (0 = success) and separate error message retrieval

### Migration Path

For existing ClojureScript applications:
1. **No Changes Required**: Use `WasmEditorCore` for drop-in compatibility
2. **Future Enhancement**: Gradually migrate to `EditorState` for better performance and immutability

### WASM Loading

```javascript
import init, { WasmEditorCore } from './pkg/core_wasm.js';

await init();
const editor = new WasmEditorCore();
editor.init("Initial text content");
```

## Performance Characteristics

- **Text Insertion**: O(log k) where k is the number of pieces
- **Text Retrieval**: O(k) for full text, O(log k) for specific ranges
- **Line Navigation**: O(log k) to any line number
- **Memory Usage**: Minimal overhead, no text duplication
- **Large Files**: Excellent performance due to structural sharing

## Development Environment

This package is designed to be developed within the provided Dev Container which includes:
- Rust toolchain with WebAssembly support
- wasm-pack for building
- VS Code extensions for Rust development
- All necessary tooling pre-configured
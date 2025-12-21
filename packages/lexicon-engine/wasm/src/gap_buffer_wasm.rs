// WASM bindings for GapBuffer
//
// Provides a simple, direct interface to the gap buffer for ClojureScript

use wasm_bindgen::prelude::*;
use lexicon_core::gap_buffer::GapBuffer;

/// WASM wrapper for GapBuffer with simple, imperative API
#[wasm_bindgen]
pub struct WasmGapBuffer {
    buffer: GapBuffer,
}

#[wasm_bindgen]
impl WasmGapBuffer {
    /// Create a new buffer with optional initial text
    #[wasm_bindgen(constructor)]
    pub fn new(initial_text: &str) -> WasmGapBuffer {
        WasmGapBuffer {
            buffer: GapBuffer::new(initial_text),
        }
    }

    /// Insert text at position
    #[wasm_bindgen]
    pub fn insert(&mut self, position: usize, text: &str) {
        self.buffer.insert(position, text);
    }

    /// Delete len bytes starting at position
    #[wasm_bindgen]
    pub fn delete(&mut self, position: usize, length: usize) {
        self.buffer.delete(position, length);
    }

    /// Replace length bytes at position with new text (atomic delete+insert)
    #[wasm_bindgen]
    pub fn replace(&mut self, position: usize, length: usize, text: &str) {
        self.buffer.replace(position, length, text);
    }

    /// Get the entire text
    #[wasm_bindgen(js_name = getText)]
    pub fn get_text(&self) -> String {
        self.buffer.get_text()
    }

    /// Get text in a range
    #[wasm_bindgen(js_name = getRange)]
    pub fn get_range(&self, start: usize, end: usize) -> String {
        self.buffer.get_range(start, end)
    }

    /// Get total length (excluding gap)
    #[wasm_bindgen]
    pub fn length(&self) -> usize {
        self.buffer.len()
    }

    /// Check if buffer is empty
    #[wasm_bindgen(js_name = isEmpty)]
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }
}

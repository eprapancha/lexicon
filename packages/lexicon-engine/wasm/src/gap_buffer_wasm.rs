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

    // ========== Marker Management ==========

    /// Create a new marker at the specified position
    /// Returns the marker ID
    #[wasm_bindgen(js_name = createMarker)]
    pub fn create_marker(&mut self, position: usize) -> u64 {
        self.buffer.create_marker(position)
    }

    /// Move marker to a new position
    /// Returns true on success, false if marker not found or position invalid
    #[wasm_bindgen(js_name = moveMarker)]
    pub fn move_marker(&mut self, marker_id: u64, new_position: usize) -> bool {
        self.buffer.move_marker(marker_id, new_position).is_ok()
    }

    /// Get marker position
    /// Returns the position, or -1 if marker not found
    #[wasm_bindgen(js_name = getMarkerPosition)]
    pub fn get_marker_position(&self, marker_id: u64) -> i64 {
        self.buffer.get_marker_position(marker_id)
            .map(|pos| pos as i64)
            .unwrap_or(-1)
    }

    /// Delete a marker
    /// Returns true if marker was deleted, false if not found
    #[wasm_bindgen(js_name = deleteMarker)]
    pub fn delete_marker(&mut self, marker_id: u64) -> bool {
        self.buffer.delete_marker(marker_id)
    }
}

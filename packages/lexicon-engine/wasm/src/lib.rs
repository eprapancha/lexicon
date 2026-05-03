use wasm_bindgen::prelude::*;

// Gap Buffer WASM bindings (simple, clean API)
pub mod gap_buffer_wasm;
// Grep search WASM bindings (ripgrep search engine)
pub mod grep_wasm;

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

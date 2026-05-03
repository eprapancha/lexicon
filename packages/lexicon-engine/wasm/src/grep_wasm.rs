use lexicon_grep::{search_slice, GrepOptions};
use wasm_bindgen::prelude::*;

/// Search content for a regex pattern. Returns JSON array of matches.
#[wasm_bindgen(js_name = grepSearch)]
pub fn grep_search(
    pattern: &str,
    content: &str,
    case_insensitive: bool,
    max_count: i32,
    fixed_strings: bool,
) -> Result<String, JsValue> {
    let options = GrepOptions {
        case_insensitive,
        max_count: if max_count < 0 {
            None
        } else {
            Some(max_count as u64)
        },
        context_lines: 0,
        fixed_strings,
    };

    let matches = search_slice(pattern, content.as_bytes(), &options)
        .map_err(|e| JsValue::from_str(&e))?;

    serde_json::to_string(&matches).map_err(|e| JsValue::from_str(&e.to_string()))
}

/// Search with context lines. Returns JSON array of matches.
#[wasm_bindgen(js_name = grepSearchWithContext)]
pub fn grep_search_with_context(
    pattern: &str,
    content: &str,
    case_insensitive: bool,
    context_lines: usize,
) -> Result<String, JsValue> {
    let options = GrepOptions {
        case_insensitive,
        context_lines,
        ..Default::default()
    };

    let matches = search_slice(pattern, content.as_bytes(), &options)
        .map_err(|e| JsValue::from_str(&e))?;

    serde_json::to_string(&matches).map_err(|e| JsValue::from_str(&e.to_string()))
}

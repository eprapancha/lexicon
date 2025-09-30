use wasm_bindgen::prelude::*;
use lexicon_core::{EditorState, Transaction, Change};
use serde::{Deserialize, Serialize};

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
    
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

// WebAssembly wrapper for EditorState
#[wasm_bindgen]
pub struct WasmEditorState {
    inner: EditorState,
}

#[wasm_bindgen]
impl WasmEditorState {
    #[wasm_bindgen(constructor)]
    pub fn new(initial_text: &str) -> WasmEditorState {
        WasmEditorState {
            inner: EditorState::new(initial_text),
        }
    }
    
    #[wasm_bindgen]
    pub fn apply_transaction(&self, transaction_json: &str) -> Result<WasmEditorState, JsValue> {
        let transaction: JsonTransaction = serde_json::from_str(transaction_json)
            .map_err(|e| JsValue::from_str(&format!("Failed to parse transaction: {}", e)))?;
        
        // Convert JSON transaction to core transaction
        let core_transaction = Transaction {
            changes: transaction.changes.into_iter().map(|c| Change {
                from: c.from,
                to: c.to,
                insert: c.insert,
            }).collect(),
        };
        
        let new_state = self.inner.apply_transaction(&core_transaction);
        
        Ok(WasmEditorState {
            inner: new_state,
        })
    }
    
    #[wasm_bindgen]
    pub fn get_text_range(&self, from: usize, to: usize) -> String {
        self.inner.get_text_range(from, to)
    }
    
    #[wasm_bindgen]
    pub fn get_text(&self) -> String {
        self.inner.get_text()
    }
    
    #[wasm_bindgen]
    pub fn length(&self) -> usize {
        self.inner.length()
    }
    
    #[wasm_bindgen]
    pub fn line_count(&self) -> usize {
        self.inner.line_count()
    }
    
    #[wasm_bindgen]
    pub fn version(&self) -> u64 {
        self.inner.version()
    }
}

// JSON structures for serialization
#[derive(Debug, Deserialize)]
struct JsonTransaction {
    changes: Vec<JsonChange>,
}

#[derive(Debug, Deserialize)]
struct JsonChange {
    from: usize,
    to: usize,
    insert: String,
}

// ClojureScript transaction format
#[derive(Debug, Deserialize)]
struct ClojureScriptTransaction {
    #[serde(rename = "type")]
    transaction_type: i32,
    position: usize,
    text: Option<String>,
    length: Option<usize>,
}

// Compatibility layer for ClojureScript API expectations
#[wasm_bindgen]
pub struct WasmEditorCore {
    current_state: EditorState,
    last_result: String,
    last_error: String,
}

#[wasm_bindgen]
impl WasmEditorCore {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmEditorCore {
        WasmEditorCore {
            current_state: EditorState::new(""),
            last_result: String::new(),
            last_error: String::new(),
        }
    }
    
    #[wasm_bindgen]
    pub fn init(&mut self, initial_text: &str) {
        self.current_state = EditorState::new(initial_text);
        self.last_result.clear();
        self.last_error.clear();
    }
    
    // Enhanced ClojureScript API: applyTransaction(json) -> Result<String, JsValue>
    #[wasm_bindgen(js_name = applyTransaction)]
    pub fn apply_transaction(&mut self, transaction_json: &str) -> Result<String, JsValue> {
        // Convert ClojureScript transaction format to internal format
        match self.convert_cljs_transaction(transaction_json) {
            Ok(internal_json) => {
                match self.apply_internal_transaction(&internal_json) {
                    Ok(new_state) => {
                        let old_state = self.current_state.clone();
                        self.current_state = new_state;
                        
                        // Parse the original transaction for patch generation
                        let cljs_transaction: Result<ClojureScriptTransaction, _> = 
                            serde_json::from_str(transaction_json);
                        
                        if let Ok(transaction) = cljs_transaction {
                            // Generate patch describing the change
                            let patch = match transaction.transaction_type {
                                0 => {
                                    // Insert patch
                                    format!(
                                        r#"{{"type": "edit", "operation": "insert", "start_line": {}, "end_line": {}, "position": {}, "text": "{}", "version": {}, "length": {}}}"#,
                                        self.position_to_line(transaction.position),
                                        self.position_to_line(transaction.position + transaction.text.as_ref().map_or(0, |t| t.len())),
                                        transaction.position,
                                        transaction.text.as_ref().unwrap_or(&String::new()).replace('\\', "\\\\").replace('"', "\\\""),
                                        self.current_state.version(),
                                        self.current_state.length()
                                    )
                                }
                                1 => {
                                    // Delete patch
                                    let length = transaction.length.unwrap_or(0);
                                    format!(
                                        r#"{{"type": "edit", "operation": "delete", "start_line": {}, "end_line": {}, "position": {}, "length": {}, "version": {}, "length": {}}}"#,
                                        self.position_to_line(transaction.position),
                                        self.position_to_line(transaction.position + length),
                                        transaction.position,
                                        length,
                                        self.current_state.version(),
                                        self.current_state.length()
                                    )
                                }
                                2 => {
                                    // Replace patch
                                    let length = transaction.length.unwrap_or(0);
                                    format!(
                                        r#"{{"type": "edit", "operation": "replace", "start_line": {}, "end_line": {}, "position": {}, "length": {}, "text": "{}", "version": {}, "length": {}}}"#,
                                        self.position_to_line(transaction.position),
                                        self.position_to_line(transaction.position + length),
                                        transaction.position,
                                        length,
                                        transaction.text.as_ref().unwrap_or(&String::new()).replace('\\', "\\\\").replace('"', "\\\""),
                                        self.current_state.version(),
                                        self.current_state.length()
                                    )
                                }
                                _ => {
                                    format!(
                                        r#"{{"type": "edit", "operation": "unknown", "version": {}, "length": {}}}"#,
                                        self.current_state.version(),
                                        self.current_state.length()
                                    )
                                }
                            };
                            
                            self.last_result = patch.clone();
                            self.last_error.clear();
                            Ok(patch)
                        } else {
                            let fallback_patch = format!(
                                r#"{{"type": "edit", "operation": "unknown", "version": {}, "length": {}}}"#,
                                self.current_state.version(),
                                self.current_state.length()
                            );
                            self.last_result = fallback_patch.clone();
                            Ok(fallback_patch)
                        }
                    }
                    Err(e) => {
                        let error_msg = format!("Transaction failed: {}", e);
                        self.last_error = error_msg.clone();
                        self.last_result.clear();
                        Err(JsValue::from_str(&error_msg))
                    }
                }
            }
            Err(e) => {
                let error_msg = format!("Invalid transaction format: {}", e);
                self.last_error = error_msg.clone();
                self.last_result.clear();
                Err(JsValue::from_str(&error_msg))
            }
        }
    }
    
    #[wasm_bindgen(js_name = getLastResult)]
    pub fn get_last_result(&self) -> String {
        self.last_result.clone()
    }
    
    #[wasm_bindgen(js_name = getLastErrorMessage)]
    pub fn get_last_error_message(&self) -> String {
        self.last_error.clone()
    }
    
    #[wasm_bindgen(js_name = getText)]
    pub fn get_text(&self) -> String {
        self.current_state.get_text()
    }
    
    #[wasm_bindgen(js_name = getLength)]
    pub fn get_length(&self) -> usize {
        self.current_state.length()
    }
    
    #[wasm_bindgen(js_name = getTextInRange)]
    pub fn get_text_in_range(&self, start: usize, end: usize) -> String {
        self.current_state.get_text_range(start, end)
    }
    
    #[wasm_bindgen(js_name = getCharacterAt)]
    pub fn get_character_at(&self, position: usize) -> String {
        let text = self.current_state.get_text();
        let chars: Vec<char> = text.chars().collect();
        if position < chars.len() {
            chars[position].to_string()
        } else {
            String::new()
        }
    }

    #[wasm_bindgen(js_name = getTextForLineRange)]
    pub fn get_text_for_line_range(&self, start_line: usize, end_line: usize) -> String {
        let text = self.current_state.get_text();
        let lines: Vec<&str> = text.lines().collect();
        
        if start_line >= lines.len() {
            return String::new();
        }
        
        let actual_end = end_line.min(lines.len());
        
        if start_line >= actual_end {
            return String::new();
        }
        
        lines[start_line..actual_end].join("\n")
    }

    #[wasm_bindgen(js_name = lineCount)]
    pub fn line_count(&self) -> usize {
        self.current_state.line_count()
    }
    
    #[wasm_bindgen(js_name = deleteText)]
    pub fn delete_text(&mut self, start: usize, length: usize) -> i32 {
        let transaction = Transaction {
            changes: vec![Change {
                from: start,
                to: start + length,
                insert: String::new(),
            }],
        };
        
        let new_state = self.current_state.apply_transaction(&transaction);
        self.current_state = new_state;
        0
    }
    
    #[wasm_bindgen(js_name = insertText)]
    pub fn insert_text(&mut self, position: usize, text: &str) -> i32 {
        let transaction = Transaction {
            changes: vec![Change {
                from: position,
                to: position,
                insert: text.to_string(),
            }],
        };
        
        let new_state = self.current_state.apply_transaction(&transaction);
        self.current_state = new_state;
        0
    }
    
    // Internal helper methods
    fn apply_internal_transaction(&self, transaction_json: &str) -> Result<EditorState, String> {
        let transaction: JsonTransaction = serde_json::from_str(transaction_json)
            .map_err(|e| format!("Failed to parse transaction: {}", e))?;
        
        let core_transaction = Transaction {
            changes: transaction.changes.into_iter().map(|c| Change {
                from: c.from,
                to: c.to,
                insert: c.insert,
            }).collect(),
        };
        
        Ok(self.current_state.apply_transaction(&core_transaction))
    }
    
    // Explicit memory cleanup method
    #[wasm_bindgen]
    pub fn free(self) {
        // The struct will be dropped here, releasing all memory
        // This is an explicit cleanup method that can be called from ClojureScript
        console_log!("WasmEditorCore instance freed");
    }
    
    // Helper method to convert character position to line number
    fn position_to_line(&self, position: usize) -> usize {
        let text = self.current_state.get_text();
        let chars: Vec<char> = text.chars().collect();
        let mut line = 0;
        
        for (i, &ch) in chars.iter().enumerate() {
            if i >= position {
                break;
            }
            if ch == '\n' {
                line += 1;
            }
        }
        
        line
    }

    // Convert ClojureScript transaction format to internal format
    fn convert_cljs_transaction(&self, cljs_json: &str) -> Result<String, String> {
        let cljs_transaction: ClojureScriptTransaction = serde_json::from_str(cljs_json)
            .map_err(|e| format!("Failed to parse ClojureScript transaction: {}", e))?;
        
        let internal_format = match cljs_transaction.transaction_type {
            0 => {
                // Insert
                let text = cljs_transaction.text.unwrap_or_default();
                format!(
                    r#"{{"changes": [{{"from": {}, "to": {}, "insert": "{}"}}]}}"#,
                    cljs_transaction.position,
                    cljs_transaction.position,
                    text.replace('\\', "\\\\").replace('"', "\\\"")
                )
            }
            1 => {
                // Delete
                let length = cljs_transaction.length.unwrap_or(0);
                format!(
                    r#"{{"changes": [{{"from": {}, "to": {}, "insert": ""}}]}}"#,
                    cljs_transaction.position,
                    cljs_transaction.position + length
                )
            }
            2 => {
                // Replace
                let length = cljs_transaction.length.unwrap_or(0);
                let text = cljs_transaction.text.unwrap_or_default();
                format!(
                    r#"{{"changes": [{{"from": {}, "to": {}, "insert": "{}"}}]}}"#,
                    cljs_transaction.position,
                    cljs_transaction.position + length,
                    text.replace('\\', "\\\\").replace('"', "\\\"")
                )
            }
            _ => return Err(format!("Unknown transaction type: {}", cljs_transaction.transaction_type)),
        };
        
        Ok(internal_format)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_text_for_line_range() {
        let mut core = WasmEditorCore::new();
        core.init("Line 1\nLine 2\nLine 3\nLine 4");
        
        // Test getting lines 0-2 (first 2 lines)
        assert_eq!(core.get_text_for_line_range(0, 2), "Line 1\nLine 2");
        
        // Test getting lines 1-3 (middle 2 lines) 
        assert_eq!(core.get_text_for_line_range(1, 3), "Line 2\nLine 3");
        
        // Test getting all lines
        assert_eq!(core.get_text_for_line_range(0, 4), "Line 1\nLine 2\nLine 3\nLine 4");
        
        // Test out of bounds
        assert_eq!(core.get_text_for_line_range(5, 10), "");
        
        // Test single line
        assert_eq!(core.get_text_for_line_range(0, 1), "Line 1");
    }

    #[test]
    fn test_line_count() {
        let mut core = WasmEditorCore::new();
        
        // Empty text
        core.init("");
        assert_eq!(core.line_count(), 1);
        
        // Single line
        core.init("Hello");
        assert_eq!(core.line_count(), 1);
        
        // Multiple lines
        core.init("Line 1\nLine 2\nLine 3");
        assert_eq!(core.line_count(), 3);
        
        // Text ending with newline
        core.init("Line 1\nLine 2\n");
        assert_eq!(core.line_count(), 3);
    }

    #[test]
    fn test_get_text_for_line_range_edge_cases() {
        let mut core = WasmEditorCore::new();
        core.init("A\nB\nC");
        
        // Start line >= end line
        assert_eq!(core.get_text_for_line_range(2, 2), "");
        assert_eq!(core.get_text_for_line_range(2, 1), "");
        
        // Start line beyond available lines
        assert_eq!(core.get_text_for_line_range(10, 15), "");
        
        // End line beyond available lines (should clamp)
        assert_eq!(core.get_text_for_line_range(1, 10), "B\nC");
    }
}
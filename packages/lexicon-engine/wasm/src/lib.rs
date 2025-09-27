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
    
    // ClojureScript expects: applyTransaction(json) -> number (0 = success)
    #[wasm_bindgen(js_name = applyTransaction)]
    pub fn apply_transaction(&mut self, transaction_json: &str) -> i32 {
        // Convert ClojureScript transaction format to internal format
        match self.convert_cljs_transaction(transaction_json) {
            Ok(internal_json) => {
                match self.apply_internal_transaction(&internal_json) {
                    Ok(new_state) => {
                        self.current_state = new_state;
                        self.last_result = format!(
                            r#"{{"success": true, "version": {}, "length": {}}}"#,
                            self.current_state.version(),
                            self.current_state.length()
                        );
                        self.last_error.clear();
                        0 // Success
                    }
                    Err(e) => {
                        self.last_error = format!("Transaction failed: {}", e);
                        self.last_result.clear();
                        1 // Error
                    }
                }
            }
            Err(e) => {
                self.last_error = format!("Invalid transaction format: {}", e);
                self.last_result.clear();
                2 // Parse error
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
    
    // Convert ClojureScript transaction format to internal format
    fn convert_cljs_transaction(&self, cljs_json: &str) -> Result<String, String> {
        #[derive(Deserialize)]
        struct ClojureScriptTransaction {
            #[serde(rename = "type")]
            transaction_type: i32,
            position: usize,
            text: Option<String>,
            length: Option<usize>,
        }
        
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
use wasm_bindgen::prelude::*;
use std::cmp::Ordering;
use std::collections::VecDeque;
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BufferType {
    Original,
    Add,
}

#[derive(Debug, Clone)]
pub struct Piece {
    buffer_type: BufferType,
    start: usize,
    length: usize,
}

impl Piece {
    pub fn new(buffer_type: BufferType, start: usize, length: usize) -> Self {
        Piece {
            buffer_type,
            start,
            length,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NodeMetadata {
    char_count: usize,
    line_count: usize,
    left_char_count: usize,
    left_line_count: usize,
}

impl NodeMetadata {
    pub fn new() -> Self {
        NodeMetadata {
            char_count: 0,
            line_count: 0,
            left_char_count: 0,
            left_line_count: 0,
        }
    }
    
    pub fn from_piece(piece: &Piece, text: &str) -> Self {
        let piece_text = &text[piece.start..piece.start + piece.length];
        let line_count = piece_text.matches('\n').count();
        
        NodeMetadata {
            char_count: piece.length,
            line_count,
            left_char_count: 0,
            left_line_count: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    Red,
    Black,
}

#[derive(Debug, Clone)]
pub struct RBNode {
    piece: Option<Piece>,
    metadata: NodeMetadata,
    color: Color,
    left: Option<Box<RBNode>>,
    right: Option<Box<RBNode>>,
}

impl RBNode {
    pub fn new_leaf(piece: Piece, metadata: NodeMetadata) -> Self {
        RBNode {
            piece: Some(piece),
            metadata,
            color: Color::Red,
            left: None,
            right: None,
        }
    }
    
    pub fn new_internal(metadata: NodeMetadata) -> Self {
        RBNode {
            piece: None,
            metadata,
            color: Color::Black,
            left: None,
            right: None,
        }
    }
    
    pub fn is_leaf(&self) -> bool {
        self.piece.is_some()
    }
    
    pub fn update_metadata(&mut self, original_buffer: &str, add_buffer: &str) {
        if self.is_leaf() {
            if let Some(piece) = &self.piece {
                let text = match piece.buffer_type {
                    BufferType::Original => original_buffer,
                    BufferType::Add => add_buffer,
                };
                self.metadata = NodeMetadata::from_piece(piece, text);
            }
        } else {
            let mut total_char_count = 0;
            let mut total_line_count = 0;
            let mut left_char_count = 0;
            let mut left_line_count = 0;
            
            if let Some(ref left) = self.left {
                left_char_count = left.metadata.char_count;
                left_line_count = left.metadata.line_count;
                total_char_count += left_char_count;
                total_line_count += left_line_count;
            }
            
            if let Some(ref right) = self.right {
                total_char_count += right.metadata.char_count;
                total_line_count += right.metadata.line_count;
            }
            
            self.metadata.char_count = total_char_count;
            self.metadata.line_count = total_line_count;
            self.metadata.left_char_count = left_char_count;
            self.metadata.left_line_count = left_line_count;
        }
    }
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct PieceTree {
    original_buffer: String,
    add_buffer: String,
    root: Option<Box<RBNode>>,
}

#[wasm_bindgen]
impl PieceTree {
    #[wasm_bindgen(constructor)]
    pub fn new(initial_text: &str) -> PieceTree {
        let mut tree = PieceTree {
            original_buffer: initial_text.to_string(),
            add_buffer: String::new(),
            root: None,
        };
        
        if !initial_text.is_empty() {
            let piece = Piece::new(BufferType::Original, 0, initial_text.len());
            let metadata = NodeMetadata::from_piece(&piece, initial_text);
            tree.root = Some(Box::new(RBNode::new_leaf(piece, metadata)));
        }
        
        tree
    }
    
    #[wasm_bindgen]
    pub fn length(&self) -> usize {
        self.root.as_ref().map_or(0, |node| node.metadata.char_count)
    }
    
    #[wasm_bindgen]
    pub fn line_count(&self) -> usize {
        self.root.as_ref().map_or(1, |node| node.metadata.line_count + 1)
    }
    
    #[wasm_bindgen]
    pub fn insert(&mut self, position: usize, text: &str) {
        if text.is_empty() {
            return;
        }
        
        let add_start = self.add_buffer.len();
        self.add_buffer.push_str(text);
        
        let new_piece = Piece::new(BufferType::Add, add_start, text.len());
        
        if self.root.is_none() {
            let metadata = NodeMetadata::from_piece(&new_piece, &self.add_buffer);
            self.root = Some(Box::new(RBNode::new_leaf(new_piece, metadata)));
            return;
        }
        
        self.insert_at_position(position, new_piece);
    }
    
    fn insert_at_position(&mut self, position: usize, new_piece: Piece) {
        if position == 0 {
            self.insert_at_beginning(new_piece);
        } else if position >= self.length() {
            self.insert_at_end(new_piece);
        } else {
            self.insert_at_middle(position, new_piece);
        }
    }
    
    fn insert_at_beginning(&mut self, new_piece: Piece) {
        let new_metadata = NodeMetadata::from_piece(&new_piece, &self.add_buffer);
        let new_node = Box::new(RBNode::new_leaf(new_piece, new_metadata));
        
        if let Some(old_root) = self.root.take() {
            let mut internal_node = RBNode::new_internal(NodeMetadata::new());
            internal_node.left = Some(new_node);
            internal_node.right = Some(old_root);
            internal_node.update_metadata(&self.original_buffer, &self.add_buffer);
            self.root = Some(Box::new(internal_node));
        } else {
            self.root = Some(new_node);
        }
    }
    
    fn insert_at_end(&mut self, new_piece: Piece) {
        let new_metadata = NodeMetadata::from_piece(&new_piece, &self.add_buffer);
        let new_node = Box::new(RBNode::new_leaf(new_piece, new_metadata));
        
        if let Some(old_root) = self.root.take() {
            let mut internal_node = RBNode::new_internal(NodeMetadata::new());
            internal_node.left = Some(old_root);
            internal_node.right = Some(new_node);
            internal_node.update_metadata(&self.original_buffer, &self.add_buffer);
            self.root = Some(Box::new(internal_node));
        } else {
            self.root = Some(new_node);
        }
    }
    
    fn insert_at_middle(&mut self, position: usize, new_piece: Piece) {
        if let Some(piece_info) = self.find_piece_at_position(position) {
            if piece_info.piece_start == position {
                self.insert_at_beginning(new_piece);
            } else if piece_info.piece_start + piece_info.piece.length == position {
                self.insert_at_end(new_piece);
            } else {
                self.split_and_insert(piece_info, position, new_piece);
            }
        }
    }
    
    fn split_and_insert(&mut self, piece_info: PieceInfo, position: usize, new_piece: Piece) {
        let split_offset = position - piece_info.piece_start;
        
        let left_piece = Piece::new(
            piece_info.piece.buffer_type,
            piece_info.piece.start,
            split_offset,
        );
        
        let right_piece = Piece::new(
            piece_info.piece.buffer_type,
            piece_info.piece.start + split_offset,
            piece_info.piece.length - split_offset,
        );
        
        let left_metadata = NodeMetadata::from_piece(&left_piece, &self.get_buffer_text(left_piece.buffer_type));
        let new_metadata = NodeMetadata::from_piece(&new_piece, &self.add_buffer);
        let right_metadata = NodeMetadata::from_piece(&right_piece, &self.get_buffer_text(right_piece.buffer_type));
        
        let left_node = Box::new(RBNode::new_leaf(left_piece, left_metadata));
        let new_node = Box::new(RBNode::new_leaf(new_piece, new_metadata));
        let right_node = Box::new(RBNode::new_leaf(right_piece, right_metadata));
        
        let mut left_internal = RBNode::new_internal(NodeMetadata::new());
        left_internal.left = Some(left_node);
        left_internal.right = Some(new_node);
        left_internal.update_metadata(&self.original_buffer, &self.add_buffer);
        
        let mut root_internal = RBNode::new_internal(NodeMetadata::new());
        root_internal.left = Some(Box::new(left_internal));
        root_internal.right = Some(right_node);
        root_internal.update_metadata(&self.original_buffer, &self.add_buffer);
        
        self.root = Some(Box::new(root_internal));
    }
    
    fn get_buffer_text(&self, buffer_type: BufferType) -> &str {
        match buffer_type {
            BufferType::Original => &self.original_buffer,
            BufferType::Add => &self.add_buffer,
        }
    }
    
    #[wasm_bindgen]
    pub fn get_text(&self) -> String {
        let mut result = String::new();
        self.collect_text(&self.root, &mut result);
        result
    }
    
    fn collect_text(&self, node: &Option<Box<RBNode>>, result: &mut String) {
        if let Some(node) = node {
            if node.is_leaf() {
                if let Some(piece) = &node.piece {
                    let buffer = self.get_buffer_text(piece.buffer_type);
                    result.push_str(&buffer[piece.start..piece.start + piece.length]);
                }
            } else {
                self.collect_text(&node.left, result);
                self.collect_text(&node.right, result);
            }
        }
    }
    
    #[wasm_bindgen]
    pub fn get_text_range(&self, from: usize, to: usize) -> String {
        if from >= to || from >= self.length() {
            return String::new();
        }
        
        let end = to.min(self.length());
        let full_text = self.get_text();
        
        // Convert byte positions to character positions for proper UTF-8 handling
        let chars: Vec<char> = full_text.chars().collect();
        if from >= chars.len() {
            return String::new();
        }
        
        let end_char = end.min(chars.len());
        chars[from..end_char].iter().collect()
    }
    
    pub fn delete_range(&mut self, from: usize, to: usize) {
        if from >= to || from >= self.length() {
            return;
        }
        
        let end = to.min(self.length());
        
        // For now, implement deletion by rebuilding the tree
        // This is not optimal but works correctly
        let full_text = self.get_text();
        let chars: Vec<char> = full_text.chars().collect();
        
        if from >= chars.len() {
            return;
        }
        
        let end_char = end.min(chars.len());
        let mut new_text = String::new();
        
        // Add text before deletion
        new_text.extend(chars[..from].iter());
        // Add text after deletion
        new_text.extend(chars[end_char..].iter());
        
        // Rebuild the tree with new text
        *self = PieceTree::new(&new_text);
    }
    
    #[wasm_bindgen]
    pub fn get_line(&self, line_number: usize) -> String {
        if let Some(line_info) = self.find_line(line_number) {
            let text = self.get_text();
            let lines: Vec<&str> = text.lines().collect();
            if line_number < lines.len() {
                lines[line_number].to_string()
            } else {
                String::new()
            }
        } else {
            String::new()
        }
    }
    
    fn find_line(&self, line_number: usize) -> Option<LineInfo> {
        self.find_line_recursive(&self.root, line_number, 0)
    }
    
    fn find_line_recursive(&self, node: &Option<Box<RBNode>>, target_line: usize, current_line: usize) -> Option<LineInfo> {
        if let Some(node) = node {
            if node.is_leaf() {
                Some(LineInfo {
                    line_start: current_line,
                    line_end: current_line + node.metadata.line_count,
                })
            } else {
                let left_line_count = node.metadata.left_line_count;
                
                if target_line < current_line + left_line_count {
                    self.find_line_recursive(&node.left, target_line, current_line)
                } else {
                    self.find_line_recursive(&node.right, target_line, current_line + left_line_count)
                }
            }
        } else {
            None
        }
    }
    
    fn find_piece_at_position(&self, position: usize) -> Option<PieceInfo> {
        self.find_piece_recursive(&self.root, position, 0)
    }
    
    fn find_piece_recursive(&self, node: &Option<Box<RBNode>>, position: usize, current_pos: usize) -> Option<PieceInfo> {
        if let Some(node) = node {
            if node.is_leaf() {
                if let Some(piece) = &node.piece {
                    if position >= current_pos && position < current_pos + piece.length {
                        return Some(PieceInfo {
                            piece: piece.clone(),
                            piece_start: current_pos,
                        });
                    }
                }
            } else {
                let left_char_count = node.metadata.left_char_count;
                
                if position < current_pos + left_char_count {
                    return self.find_piece_recursive(&node.left, position, current_pos);
                } else {
                    return self.find_piece_recursive(&node.right, position, current_pos + left_char_count);
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
struct PieceInfo {
    piece: Piece,
    piece_start: usize,
}

#[derive(Debug, Clone)]
struct LineInfo {
    line_start: usize,
    line_end: usize,
}

#[derive(Debug, Deserialize)]
pub struct Change {
    pub from: usize,
    pub to: usize,
    pub insert: String,
}

#[derive(Debug, Deserialize)]
pub struct Transaction {
    pub changes: Vec<Change>,
}

impl Transaction {
    pub fn new_insert(position: usize, text: String) -> Self {
        Transaction {
            changes: vec![Change {
                from: position,
                to: position,
                insert: text,
            }],
        }
    }
    
    pub fn new_delete(from: usize, to: usize) -> Self {
        Transaction {
            changes: vec![Change {
                from,
                to,
                insert: String::new(),
            }],
        }
    }
    
    pub fn new_replace(from: usize, to: usize, text: String) -> Self {
        Transaction {
            changes: vec![Change {
                from,
                to,
                insert: text,
            }],
        }
    }
}

#[wasm_bindgen]
pub struct EditorState {
    piece_tree: PieceTree,
    version: u64,
}

#[wasm_bindgen]
impl EditorState {
    #[wasm_bindgen(constructor)]
    pub fn new(initial_text: &str) -> EditorState {
        EditorState {
            piece_tree: PieceTree::new(initial_text),
            version: 0,
        }
    }
    
    #[wasm_bindgen]
    pub fn apply_transaction(&self, transaction_json: &str) -> Result<EditorState, JsValue> {
        let transaction: Transaction = serde_json::from_str(transaction_json)
            .map_err(|e| JsValue::from_str(&format!("Failed to parse transaction: {}", e)))?;
        
        let mut new_tree = self.piece_tree.clone();
        
        // Apply changes in reverse order to maintain position validity
        let mut changes = transaction.changes;
        changes.sort_by(|a, b| b.from.cmp(&a.from));
        
        for change in changes {
            // First delete the range if needed
            if change.to > change.from {
                new_tree.delete_range(change.from, change.to);
            }
            
            // Then insert new text if any
            if !change.insert.is_empty() {
                new_tree.insert(change.from, &change.insert);
            }
        }
        
        Ok(EditorState {
            piece_tree: new_tree,
            version: self.version + 1,
        })
    }
    
    #[wasm_bindgen]
    pub fn get_text_range(&self, from: usize, to: usize) -> String {
        self.piece_tree.get_text_range(from, to)
    }
    
    #[wasm_bindgen]
    pub fn get_text(&self) -> String {
        self.piece_tree.get_text()
    }
    
    #[wasm_bindgen]
    pub fn length(&self) -> usize {
        self.piece_tree.length()
    }
    
    #[wasm_bindgen]
    pub fn line_count(&self) -> usize {
        self.piece_tree.line_count()
    }
    
    #[wasm_bindgen]
    pub fn version(&self) -> u64 {
        self.version
    }
}

impl Clone for EditorState {
    fn clone(&self) -> Self {
        EditorState {
            piece_tree: self.piece_tree.clone(),
            version: self.version,
        }
    }
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
                match self.current_state.apply_transaction(&internal_json) {
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
                        self.last_error = format!("Transaction failed: {}", e.as_string().unwrap_or_else(|| "Unknown error".to_string()));
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
        let transaction_json = format!(
            r#"{{"changes": [{{"from": {}, "to": {}, "insert": ""}}]}}"#,
            start,
            start + length
        );
        
        match self.current_state.apply_transaction(&transaction_json) {
            Ok(new_state) => {
                self.current_state = new_state;
                0
            }
            Err(_) => 1
        }
    }
    
    #[wasm_bindgen(js_name = insertText)]
    pub fn insert_text(&mut self, position: usize, text: &str) -> i32 {
        let escaped_text = text.replace('\\', "\\\\").replace('"', "\\\"");
        let transaction_json = format!(
            r#"{{"changes": [{{"from": {}, "to": {}, "insert": "{}"}}]}}"#,
            position,
            position,
            escaped_text
        );
        
        match self.current_state.apply_transaction(&transaction_json) {
            Ok(new_state) => {
                self.current_state = new_state;
                0
            }
            Err(_) => 1
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_piece_tree() {
        let tree = PieceTree::new("Hello");
        assert_eq!(tree.get_text(), "Hello");
        assert_eq!(tree.length(), 5);
    }

    #[test]
    fn test_piece_tree_insert() {
        let mut tree = PieceTree::new("AC");
        tree.insert(1, "B");
        assert_eq!(tree.get_text(), "ABC");
        assert_eq!(tree.length(), 3);
    }

    #[test]
    fn test_piece_tree_get_text_range() {
        let tree = PieceTree::new("Hello, World!");
        assert_eq!(tree.get_text_range(0, 5), "Hello");
        assert_eq!(tree.get_text_range(7, 12), "World");
        assert_eq!(tree.get_text_range(0, 13), "Hello, World!");
    }

    #[test]
    fn test_piece_tree_delete_range() {
        let mut tree = PieceTree::new("Hello, World!");
        tree.delete_range(5, 7); // Remove ", "
        assert_eq!(tree.get_text(), "HelloWorld!");
        assert_eq!(tree.length(), 11);
    }

    #[test]
    fn test_editor_state_initialization() {
        let state = EditorState::new("AC");
        assert_eq!(state.get_text(), "AC");
        assert_eq!(state.length(), 2);
        assert_eq!(state.version(), 0);
    }

    #[test]
    fn test_editor_state_insert_transaction() {
        let state = EditorState::new("AC");
        let transaction_json = r#"{"changes": [{"from": 1, "to": 1, "insert": "B"}]}"#;
        
        let new_state = state.apply_transaction(transaction_json).unwrap();
        assert_eq!(new_state.get_text(), "ABC");
        assert_eq!(new_state.length(), 3);
        assert_eq!(new_state.version(), 1);
        
        // Original state should be unchanged
        assert_eq!(state.get_text(), "AC");
        assert_eq!(state.version(), 0);
    }

    #[test]
    fn test_editor_state_delete_transaction() {
        let state = EditorState::new("ABCD");
        let transaction_json = r#"{"changes": [{"from": 1, "to": 3, "insert": ""}]}"#;
        
        let new_state = state.apply_transaction(transaction_json).unwrap();
        assert_eq!(new_state.get_text(), "AD");
        assert_eq!(new_state.length(), 2);
        assert_eq!(new_state.version(), 1);
    }

    #[test]
    fn test_editor_state_replace_transaction() {
        let state = EditorState::new("Hello, World!");
        let transaction_json = r#"{"changes": [{"from": 7, "to": 12, "insert": "Rust"}]}"#;
        
        let new_state = state.apply_transaction(transaction_json).unwrap();
        assert_eq!(new_state.get_text(), "Hello, Rust!");
        assert_eq!(new_state.length(), 13);
        assert_eq!(new_state.version(), 1);
    }

    #[test]
    fn test_editor_state_multiple_changes() {
        let state = EditorState::new("ABCD");
        let transaction_json = r#"{"changes": [
            {"from": 1, "to": 1, "insert": "X"},
            {"from": 3, "to": 3, "insert": "Y"}
        ]}"#;
        
        let new_state = state.apply_transaction(transaction_json).unwrap();
        // Changes applied in reverse order: position 3 first, then position 1
        assert_eq!(new_state.get_text(), "AXBYCXD");
    }

    #[test]
    fn test_editor_state_get_text_range() {
        let state = EditorState::new("Hello, World!");
        assert_eq!(state.get_text_range(0, 5), "Hello");
        assert_eq!(state.get_text_range(7, 12), "World");
        assert_eq!(state.get_text_range(0, 13), "Hello, World!");
    }

    #[test]
    fn test_editor_state_sequential_transactions() {
        let state1 = EditorState::new("AC");
        let transaction1 = r#"{"changes": [{"from": 1, "to": 1, "insert": "B"}]}"#;
        let state2 = state1.apply_transaction(transaction1).unwrap();
        assert_eq!(state2.get_text(), "ABC");

        let transaction2 = r#"{"changes": [{"from": 3, "to": 3, "insert": "D"}]}"#;
        let state3 = state2.apply_transaction(transaction2).unwrap();
        assert_eq!(state3.get_text(), "ABCD");
        assert_eq!(state3.version(), 2);
    }

    #[test]
    fn test_line_operations() {
        let state = EditorState::new("Line 1\nLine 2\nLine 3");
        assert_eq!(state.line_count(), 3);
        
        let transaction = r#"{"changes": [{"from": 7, "to": 7, "insert": "New "}]}"#;
        let new_state = state.apply_transaction(transaction).unwrap();
        assert_eq!(new_state.get_text(), "Line 1\nNew Line 2\nLine 3");
    }

    #[test]
    fn test_invalid_transaction_json() {
        let state = EditorState::new("AC");
        let invalid_json = r#"{"invalid": "json"}"#;
        
        let result = state.apply_transaction(invalid_json);
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_text_operations() {
        let state = EditorState::new("");
        assert_eq!(state.length(), 0);
        assert_eq!(state.get_text(), "");
        
        let transaction = r#"{"changes": [{"from": 0, "to": 0, "insert": "Hello"}]}"#;
        let new_state = state.apply_transaction(transaction).unwrap();
        assert_eq!(new_state.get_text(), "Hello");
        assert_eq!(new_state.length(), 5);
    }

    // Tests for ClojureScript compatibility layer
    #[test]
    fn test_wasm_editor_core_initialization() {
        let mut core = WasmEditorCore::new();
        core.init("Hello, World!");
        assert_eq!(core.get_text(), "Hello, World!");
        assert_eq!(core.get_length(), 13);
    }

    #[test]
    fn test_cljs_insert_transaction() {
        let mut core = WasmEditorCore::new();
        core.init("AC");
        
        let cljs_transaction = r#"{"type": 0, "position": 1, "text": "B"}"#;
        let result = core.apply_transaction(cljs_transaction);
        
        assert_eq!(result, 0); // Success
        assert_eq!(core.get_text(), "ABC");
        assert!(!core.get_last_result().is_empty());
        assert!(core.get_last_error_message().is_empty());
    }

    #[test]
    fn test_cljs_delete_transaction() {
        let mut core = WasmEditorCore::new();
        core.init("ABCD");
        
        let cljs_transaction = r#"{"type": 1, "position": 1, "length": 2}"#;
        let result = core.apply_transaction(cljs_transaction);
        
        assert_eq!(result, 0); // Success
        assert_eq!(core.get_text(), "AD");
    }

    #[test]
    fn test_cljs_replace_transaction() {
        let mut core = WasmEditorCore::new();
        core.init("Hello, World!");
        
        let cljs_transaction = r#"{"type": 2, "position": 7, "length": 5, "text": "Rust"}"#;
        let result = core.apply_transaction(cljs_transaction);
        
        assert_eq!(result, 0); // Success
        assert_eq!(core.get_text(), "Hello, Rust!");
    }

    #[test]
    fn test_direct_mutation_methods() {
        let mut core = WasmEditorCore::new();
        core.init("Hello, World!");
        
        // Test insertText
        let result = core.insert_text(7, "Amazing ");
        assert_eq!(result, 0);
        assert_eq!(core.get_text(), "Hello, Amazing World!");
        
        // Test deleteText
        let result = core.delete_text(7, 8); // Remove "Amazing "
        assert_eq!(result, 0);
        assert_eq!(core.get_text(), "Hello, World!");
    }

    #[test]
    fn test_text_access_methods() {
        let mut core = WasmEditorCore::new();
        core.init("Hello, World!");
        
        assert_eq!(core.get_text_in_range(0, 5), "Hello");
        assert_eq!(core.get_text_in_range(7, 12), "World");
        assert_eq!(core.get_character_at(0), "H");
        assert_eq!(core.get_character_at(7), "W");
    }

    #[test]
    fn test_invalid_cljs_transaction() {
        let mut core = WasmEditorCore::new();
        core.init("Hello");
        
        let invalid_transaction = r#"{"invalid": "format"}"#;
        let result = core.apply_transaction(invalid_transaction);
        
        assert_ne!(result, 0); // Should fail
        assert!(!core.get_last_error_message().is_empty());
        assert!(core.get_last_result().is_empty());
    }
}
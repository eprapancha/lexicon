use wasm_bindgen::prelude::*;
use std::cmp::Ordering;
use std::collections::VecDeque;

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
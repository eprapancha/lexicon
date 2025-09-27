// Core data structures for the text editor

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BufferType {
    Original,
    Add,
}

#[derive(Debug, Clone)]
pub struct Piece {
    pub buffer_type: BufferType,
    pub start: usize,
    pub length: usize,
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
    pub char_count: usize,
    pub line_count: usize,
    pub left_char_count: usize,
    pub left_line_count: usize,
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
    pub piece: Option<Piece>,
    pub metadata: NodeMetadata,
    pub color: Color,
    pub left: Option<Box<RBNode>>,
    pub right: Option<Box<RBNode>>,
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

#[derive(Clone)]
pub struct PieceTree {
    pub original_buffer: String,
    pub add_buffer: String,
    pub root: Option<Box<RBNode>>,
}

impl PieceTree {
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
    
    pub fn length(&self) -> usize {
        self.root.as_ref().map_or(0, |node| node.metadata.char_count)
    }
    
    pub fn line_count(&self) -> usize {
        self.root.as_ref().map_or(1, |node| node.metadata.line_count + 1)
    }
    
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
    
    pub fn get_line(&self, line_number: usize) -> String {
        if let Some(_line_info) = self.find_line(line_number) {
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
pub struct PieceInfo {
    pub piece: Piece,
    pub piece_start: usize,
}

#[derive(Debug, Clone)]
pub struct LineInfo {
    pub line_start: usize,
    pub line_end: usize,
}

#[derive(Debug, Clone)]
pub struct Change {
    pub from: usize,
    pub to: usize,
    pub insert: String,
}

#[derive(Debug, Clone)]
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

pub struct EditorState {
    pub piece_tree: PieceTree,
    pub version: u64,
}

impl EditorState {
    pub fn new(initial_text: &str) -> EditorState {
        EditorState {
            piece_tree: PieceTree::new(initial_text),
            version: 0,
        }
    }
    
    pub fn apply_transaction(&self, transaction: &Transaction) -> EditorState {
        let mut new_tree = self.piece_tree.clone();
        
        // Apply changes in reverse order to maintain position validity
        let mut changes = transaction.changes.clone();
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
        
        EditorState {
            piece_tree: new_tree,
            version: self.version + 1,
        }
    }
    
    pub fn get_text_range(&self, from: usize, to: usize) -> String {
        self.piece_tree.get_text_range(from, to)
    }
    
    pub fn get_text(&self) -> String {
        self.piece_tree.get_text()
    }
    
    pub fn length(&self) -> usize {
        self.piece_tree.length()
    }
    
    pub fn line_count(&self) -> usize {
        self.piece_tree.line_count()
    }
    
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
        let transaction = Transaction {
            changes: vec![Change { from: 1, to: 1, insert: "B".to_string() }]
        };
        
        let new_state = state.apply_transaction(&transaction);
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
        let transaction = Transaction {
            changes: vec![Change { from: 1, to: 3, insert: "".to_string() }]
        };
        
        let new_state = state.apply_transaction(&transaction);
        assert_eq!(new_state.get_text(), "AD");
        assert_eq!(new_state.length(), 2);
        assert_eq!(new_state.version(), 1);
    }

    #[test]
    fn test_editor_state_replace_transaction() {
        let state = EditorState::new("Hello, World!");
        let transaction = Transaction {
            changes: vec![Change { from: 7, to: 12, insert: "Rust".to_string() }]
        };
        
        let new_state = state.apply_transaction(&transaction);
        assert_eq!(new_state.get_text(), "Hello, Rust!");
        assert_eq!(new_state.length(), 13);
        assert_eq!(new_state.version(), 1);
    }

    #[test]
    fn test_editor_state_multiple_changes() {
        let state = EditorState::new("ABCD");
        let transaction = Transaction {
            changes: vec![
                Change { from: 1, to: 1, insert: "X".to_string() },
                Change { from: 3, to: 3, insert: "Y".to_string() }
            ]
        };
        
        let new_state = state.apply_transaction(&transaction);
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
        let transaction1 = Transaction {
            changes: vec![Change { from: 1, to: 1, insert: "B".to_string() }]
        };
        let state2 = state1.apply_transaction(&transaction1);
        assert_eq!(state2.get_text(), "ABC");

        let transaction2 = Transaction {
            changes: vec![Change { from: 3, to: 3, insert: "D".to_string() }]
        };
        let state3 = state2.apply_transaction(&transaction2);
        assert_eq!(state3.get_text(), "ABCD");
        assert_eq!(state3.version(), 2);
    }

    #[test]
    fn test_line_operations() {
        let state = EditorState::new("Line 1\nLine 2\nLine 3");
        assert_eq!(state.line_count(), 3);
        
        let transaction = Transaction {
            changes: vec![Change { from: 7, to: 7, insert: "New ".to_string() }]
        };
        let new_state = state.apply_transaction(&transaction);
        assert_eq!(new_state.get_text(), "Line 1\nNew Line 2\nLine 3");
    }

    #[test]
    fn test_empty_text_operations() {
        let state = EditorState::new("");
        assert_eq!(state.length(), 0);
        assert_eq!(state.get_text(), "");
        
        let transaction = Transaction {
            changes: vec![Change { from: 0, to: 0, insert: "Hello".to_string() }]
        };
        let new_state = state.apply_transaction(&transaction);
        assert_eq!(new_state.get_text(), "Hello");
        assert_eq!(new_state.length(), 5);
    }
}
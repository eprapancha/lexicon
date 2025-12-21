// Gap Buffer Implementation
//
// A gap buffer is a dynamic array with an invisible "gap" at the editing point.
// This matches GNU Emacs's text representation and is optimized for localized edits.
//
// Visual representation:
// [text before gap][    GAP    ][text after gap]
//                  ^gap_start   ^gap_end
//
// Operations:
// - Insert: Fill gap at gap_start, reduce gap size
// - Delete: Expand gap to include deleted text
// - Move gap: Use memmove to reposition gap to new editing location

const INITIAL_GAP_SIZE: usize = 256; // Start with 256 bytes of gap

#[derive(Clone, Debug)]
pub struct GapBuffer {
    /// The buffer containing both text and gap
    /// Layout: [text before gap][gap][text after gap]
    buffer: Vec<u8>,

    /// Position where gap starts (inclusive)
    gap_start: usize,

    /// Position where gap ends (exclusive)
    gap_end: usize,
}

impl GapBuffer {
    /// Create a new gap buffer with optional initial text
    pub fn new(initial_text: &str) -> Self {
        let initial_bytes = initial_text.as_bytes();
        let initial_len = initial_bytes.len();

        // Allocate buffer with initial text + gap
        let total_capacity = initial_len + INITIAL_GAP_SIZE;
        let mut buffer = Vec::with_capacity(total_capacity);

        // Copy initial text
        buffer.extend_from_slice(initial_bytes);

        // Fill gap with zeros (not strictly necessary, but helpful for debugging)
        buffer.resize(total_capacity, 0);

        GapBuffer {
            buffer,
            gap_start: initial_len,
            gap_end: total_capacity,
        }
    }

    /// Get the current gap size
    #[inline]
    pub fn gap_size(&self) -> usize {
        self.gap_end - self.gap_start
    }

    /// Get the total text length (excluding gap)
    #[inline]
    pub fn len(&self) -> usize {
        self.buffer.len() - self.gap_size()
    }

    /// Check if buffer is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Move the gap to a specific position
    /// This is the core operation that makes gap buffers efficient
    pub fn move_gap_to(&mut self, position: usize) {
        if position > self.len() {
            panic!("Position {} out of bounds (len: {})", position, self.len());
        }

        if position == self.gap_start {
            // Gap is already at the target position
            return;
        }

        if position < self.gap_start {
            // Move gap backward
            // Copy text from [position..gap_start] to [gap_end - distance..gap_end]
            let distance = self.gap_start - position;
            self.buffer.copy_within(position..self.gap_start, self.gap_end - distance);
            self.gap_end -= distance;
            self.gap_start = position;
        } else {
            // Move gap forward
            // position > gap_start, so we need to account for the gap
            let distance = position - self.gap_start;
            self.buffer.copy_within(self.gap_end..self.gap_end + distance, self.gap_start);
            self.gap_start += distance;
            self.gap_end += distance;
        }
    }

    /// Ensure the gap has at least min_size bytes
    fn ensure_gap_size(&mut self, min_size: usize) {
        if self.gap_size() >= min_size {
            return;
        }

        // Calculate new capacity (double the current size or add what's needed, whichever is larger)
        let current_capacity = self.buffer.len();
        let needed_growth = min_size - self.gap_size();
        let new_capacity = current_capacity.max(current_capacity * 2).max(current_capacity + needed_growth + INITIAL_GAP_SIZE);

        // Allocate new buffer
        let mut new_buffer = Vec::with_capacity(new_capacity);

        // Copy text before gap
        new_buffer.extend_from_slice(&self.buffer[..self.gap_start]);

        // Calculate new gap size
        let text_after_gap = self.buffer.len() - self.gap_end;
        let new_gap_size = new_capacity - self.gap_start - text_after_gap;

        // Fill new gap
        new_buffer.resize(self.gap_start + new_gap_size, 0);

        // Copy text after gap
        new_buffer.extend_from_slice(&self.buffer[self.gap_end..]);

        // Update state
        self.buffer = new_buffer;
        self.gap_end = self.gap_start + new_gap_size;
    }

    /// Insert text at the current gap position
    pub fn insert(&mut self, position: usize, text: &str) {
        let bytes = text.as_bytes();
        let len = bytes.len();

        if len == 0 {
            return;
        }

        // Move gap to insertion point
        self.move_gap_to(position);

        // Ensure gap is large enough
        self.ensure_gap_size(len);

        // Copy bytes into gap
        self.buffer[self.gap_start..self.gap_start + len].copy_from_slice(bytes);

        // Advance gap start
        self.gap_start += len;
    }

    /// Delete len bytes starting at position
    pub fn delete(&mut self, position: usize, len: usize) {
        if len == 0 {
            return;
        }

        let text_len = self.len();
        if position + len > text_len {
            panic!("Delete range {}..{} out of bounds (len: {})", position, position + len, text_len);
        }

        // Move gap to deletion point
        self.move_gap_to(position);

        // Expand gap to include deleted text
        self.gap_end += len;
    }

    /// Replace len bytes starting at position with new text
    /// This is an atomic operation that combines delete and insert
    pub fn replace(&mut self, position: usize, len: usize, text: &str) {
        let text_len = self.len();
        if position + len > text_len {
            panic!("Replace range {}..{} out of bounds (len: {})", position, position + len, text_len);
        }

        let bytes = text.as_bytes();
        let new_len = bytes.len();

        // Move gap to replacement point
        self.move_gap_to(position);

        // Expand gap to include text to be replaced
        self.gap_end += len;

        // Ensure gap is large enough for new text
        self.ensure_gap_size(new_len);

        // Copy new bytes into gap
        self.buffer[self.gap_start..self.gap_start + new_len].copy_from_slice(bytes);

        // Advance gap start
        self.gap_start += new_len;
    }

    /// Get the entire text as a String
    pub fn get_text(&self) -> String {
        let mut result = Vec::with_capacity(self.len());
        result.extend_from_slice(&self.buffer[..self.gap_start]);
        result.extend_from_slice(&self.buffer[self.gap_end..]);

        // This should always be valid UTF-8 since we only insert valid UTF-8
        String::from_utf8(result).expect("Gap buffer contains invalid UTF-8")
    }

    /// Get text in a specific range
    pub fn get_range(&self, start: usize, end: usize) -> String {
        if start > end {
            panic!("Invalid range: start {} > end {}", start, end);
        }

        if end > self.len() {
            panic!("Range end {} out of bounds (len: {})", end, self.len());
        }

        let mut result = Vec::with_capacity(end - start);

        if end <= self.gap_start {
            // Range is entirely before gap
            result.extend_from_slice(&self.buffer[start..end]);
        } else if start >= self.gap_start {
            // Range is entirely after gap
            let physical_start = start + self.gap_size();
            let physical_end = end + self.gap_size();
            result.extend_from_slice(&self.buffer[physical_start..physical_end]);
        } else {
            // Range spans the gap
            result.extend_from_slice(&self.buffer[start..self.gap_start]);
            let remaining = end - self.gap_start;
            result.extend_from_slice(&self.buffer[self.gap_end..self.gap_end + remaining]);
        }

        String::from_utf8(result).expect("Gap buffer range contains invalid UTF-8")
    }

    /// Get character at position (for debugging/testing)
    pub fn char_at(&self, position: usize) -> char {
        let text = self.get_range(position, position + 1);
        text.chars().next().expect("Position out of bounds")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_empty() {
        let buf = GapBuffer::new("");
        assert_eq!(buf.len(), 0);
        assert!(buf.is_empty());
        assert_eq!(buf.get_text(), "");
    }

    #[test]
    fn test_new_with_text() {
        let buf = GapBuffer::new("Hello, World!");
        assert_eq!(buf.len(), 13);
        assert!(!buf.is_empty());
        assert_eq!(buf.get_text(), "Hello, World!");
    }

    #[test]
    fn test_insert_at_start() {
        let mut buf = GapBuffer::new("World");
        buf.insert(0, "Hello, ");
        assert_eq!(buf.get_text(), "Hello, World");
    }

    #[test]
    fn test_insert_at_end() {
        let mut buf = GapBuffer::new("Hello");
        buf.insert(5, ", World!");
        assert_eq!(buf.get_text(), "Hello, World!");
    }

    #[test]
    fn test_insert_in_middle() {
        let mut buf = GapBuffer::new("Hello World");
        buf.insert(5, ",");
        assert_eq!(buf.get_text(), "Hello, World");
    }

    #[test]
    fn test_multiple_inserts() {
        let mut buf = GapBuffer::new("");
        buf.insert(0, "H");
        buf.insert(1, "e");
        buf.insert(2, "l");
        buf.insert(3, "l");
        buf.insert(4, "o");
        assert_eq!(buf.get_text(), "Hello");
    }

    #[test]
    fn test_delete_at_start() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.delete(0, 7);
        assert_eq!(buf.get_text(), "World!");
    }

    #[test]
    fn test_delete_at_end() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.delete(5, 8);
        assert_eq!(buf.get_text(), "Hello");
    }

    #[test]
    fn test_delete_in_middle() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.delete(5, 2);
        assert_eq!(buf.get_text(), "HelloWorld!");
    }

    #[test]
    fn test_move_gap_forward() {
        let mut buf = GapBuffer::new("Hello");
        assert_eq!(buf.gap_start, 5);
        buf.move_gap_to(2);
        assert_eq!(buf.gap_start, 2);
        assert_eq!(buf.get_text(), "Hello");
    }

    #[test]
    fn test_move_gap_backward() {
        let mut buf = GapBuffer::new("Hello");
        buf.move_gap_to(2);
        buf.move_gap_to(4);
        assert_eq!(buf.gap_start, 4);
        assert_eq!(buf.get_text(), "Hello");
    }

    #[test]
    fn test_get_range() {
        let buf = GapBuffer::new("Hello, World!");
        assert_eq!(buf.get_range(0, 5), "Hello");
        assert_eq!(buf.get_range(7, 12), "World");
        assert_eq!(buf.get_range(0, 13), "Hello, World!");
    }

    #[test]
    fn test_utf8_emoji() {
        let mut buf = GapBuffer::new("Hello ");
        buf.insert(6, "👋");
        assert_eq!(buf.get_text(), "Hello 👋");

        // Insert space and another emoji
        let current_len = buf.len();
        buf.insert(current_len, " 🌍");
        assert_eq!(buf.get_text(), "Hello 👋 🌍");
        assert!(buf.len() > 10); // Emoji take multiple bytes
    }

    #[test]
    fn test_realistic_editing() {
        let mut buf = GapBuffer::new("");

        // Type "abc"
        buf.insert(0, "a");
        buf.insert(1, "b");
        buf.insert(2, "c");
        assert_eq!(buf.get_text(), "abc");

        // Move cursor to position 1 and type "X"
        buf.insert(1, "X");
        assert_eq!(buf.get_text(), "aXbc");

        // Delete "X"
        buf.delete(1, 1);
        assert_eq!(buf.get_text(), "abc");

        // Add newline and more text
        buf.insert(3, "\ndef");
        assert_eq!(buf.get_text(), "abc\ndef");
    }

    #[test]
    fn test_gap_reallocation() {
        let mut buf = GapBuffer::new("x");
        let long_text = "a".repeat(1000);
        buf.insert(1, &long_text);
        assert_eq!(buf.len(), 1001);
        assert!(buf.get_text().starts_with("x"));
        assert!(buf.get_text().ends_with("a"));
    }

    #[test]
    fn test_replace_same_length() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.replace(7, 5, "Rust!");
        assert_eq!(buf.get_text(), "Hello, Rust!!");
    }

    #[test]
    fn test_replace_shorter() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.replace(7, 5, "Go");
        assert_eq!(buf.get_text(), "Hello, Go!");
    }

    #[test]
    fn test_replace_longer() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.replace(7, 5, "JavaScript");
        assert_eq!(buf.get_text(), "Hello, JavaScript!");
    }

    #[test]
    fn test_replace_at_start() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.replace(0, 5, "Hi");
        assert_eq!(buf.get_text(), "Hi, World!");
    }

    #[test]
    fn test_replace_at_end() {
        let mut buf = GapBuffer::new("Hello, World!");
        buf.replace(7, 6, "Earth!");
        assert_eq!(buf.get_text(), "Hello, Earth!");
    }
}

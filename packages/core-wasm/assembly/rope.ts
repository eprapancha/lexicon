// High-performance Rope data structure for text manipulation
// UTF-16 based, height-balanced, with unified node type

import { allocBlock, freeBlock, allocRopeNode, freeRopeNode } from "./memory";

// Target chunk size for text fragments (512-1024 characters)
const TARGET_CHUNK_SIZE: u32 = 512;
const MIN_CHUNK_SIZE: u32 = 256; // Half of target for split decisions
const MAX_CHUNK_SIZE: u32 = 1024; // Maximum before forced split

// Result class for split operations
@unmanaged
class SplitResult {
  left: RopeNode | null;
  right: RopeNode | null;
  
  constructor(left: RopeNode | null, right: RopeNode | null) {
    this.left = left;
    this.right = right;
  }
}

// RopeNode structure - unified type for both leaf and internal nodes
// Total size: ~33 bytes + padding = 64 bytes (cache line aligned)
@unmanaged
class RopeNode {
  left: usize;      // Pointer to left child (0 if leaf or no left child)
  right: usize;     // Pointer to right child (0 if leaf or no right child)
  text: usize;      // Pointer to text buffer (0 for internal nodes)
  length: u32;      // Total text length (for leaf: fragment length, for internal: sum of children)
  weight: u32;      // Length of all text in left subtree (0 for leaf nodes)
  height: u8;       // Height of this subtree (for AVL balancing)
  
  constructor() {
    this.left = 0;
    this.right = 0;
    this.text = 0;
    this.length = 0;
    this.weight = 0;
    this.height = 1; // Leaf nodes start with height 1
  }
  
  // Check if this is a leaf node
  isLeaf(): bool {
    return this.left == 0 && this.right == 0;
  }
  
  // Get the left child node
  getLeft(): RopeNode | null {
    return this.left == 0 ? null : changetype<RopeNode>(this.left);
  }
  
  // Get the right child node
  getRight(): RopeNode | null {
    return this.right == 0 ? null : changetype<RopeNode>(this.right);
  }
  
  // Set left child
  setLeft(node: RopeNode | null): void {
    this.left = node ? changetype<usize>(node) : 0;
  }
  
  // Set right child
  setRight(node: RopeNode | null): void {
    this.right = node ? changetype<usize>(node) : 0;
  }
  
  // Update height based on children heights
  updateHeight(): void {
    const leftHeight = this.getLeft() ? this.getLeft()!.height : 0;
    const rightHeight = this.getRight() ? this.getRight()!.height : 0;
    this.height = max(leftHeight, rightHeight) + 1;
  }
  
  // Get balance factor for AVL balancing
  getBalance(): i32 {
    const leftHeight = this.getLeft() ? this.getLeft()!.height : 0;
    const rightHeight = this.getRight() ? this.getRight()!.height : 0;
    return leftHeight - rightHeight;
  }
  
  // Update weight (sum of left subtree lengths)
  updateWeight(): void {
    if (this.isLeaf()) {
      this.weight = 0; // Leaf nodes don't have weight
    } else {
      this.weight = this.getLeft() ? this.getLeft()!.length : 0;
    }
  }
  
  // Update length (total length of this subtree)
  updateLength(): void {
    if (this.isLeaf()) {
      // Length is already set when text is assigned
      return;
    } else {
      const leftLength = this.getLeft() ? this.getLeft()!.length : 0;
      const rightLength = this.getRight() ? this.getRight()!.length : 0;
      this.length = leftLength + rightLength;
    }
  }
  
  // Update all metadata (weight, length, height)
  updateMetadata(): void {
    this.updateLength();
    this.updateWeight();
    this.updateHeight();
  }
}

// Create a new leaf node with text content
function createLeafNode(text: string): RopeNode {
  const node = changetype<RopeNode>(allocRopeNode());
  
  if (text.length == 0) {
    node.text = 0;
    node.length = 0;
    node.weight = 0;
    node.height = 1;
    node.left = 0;
    node.right = 0;
    return node;
  }
  
  // Allocate memory for the text (add some padding for safety)
  const textLength = text.length * 2; // UTF-16 uses 2 bytes per code unit
  const textBuffer = allocBlock((textLength + 4) as u32); // Add 4 bytes padding
  
  if (textBuffer == 0) {
    // Failed to allocate memory
    freeRopeNode(changetype<usize>(node));
    return changetype<RopeNode>(0);
  }
  
  // Copy text to buffer
  for (let i = 0; i < text.length; i++) {
    store<u16>(textBuffer + i * 2, text.charCodeAt(i) as u16);
  }
  
  node.text = textBuffer;
  node.length = text.length as u32;
  node.weight = 0; // Leaf nodes have no weight
  node.height = 1;
  node.left = 0;
  node.right = 0;
  
  return node;
}

// Create a new internal node with left and right children
function createInternalNode(left: RopeNode | null, right: RopeNode | null): RopeNode {
  const node = changetype<RopeNode>(allocRopeNode());
  
  node.setLeft(left);
  node.setRight(right);
  node.text = 0; // Internal nodes don't have text
  node.updateMetadata();
  
  return node;
}

// Free a node and its text buffer (if it's a leaf)
function freeNode(node: RopeNode): void {
  if (node.isLeaf() && node.text != 0) {
    // Free the text buffer (including padding)
    const textSize = node.length * 2 + 4; // UTF-16 bytes + padding
    freeBlock(node.text, textSize);
  }
  
  // Free the node itself
  freeRopeNode(changetype<usize>(node));
}

// Right rotation for AVL balancing
function rotateRight(y: RopeNode): RopeNode {
  const x = y.getLeft()!;
  const T2 = x.getRight();
  
  // Perform rotation
  x.setRight(y);
  y.setLeft(T2);
  
  // Update metadata
  y.updateMetadata();
  x.updateMetadata();
  
  return x;
}

// Left rotation for AVL balancing
function rotateLeft(x: RopeNode): RopeNode {
  const y = x.getRight()!;
  const T2 = y.getLeft();
  
  // Perform rotation
  y.setLeft(x);
  x.setRight(T2);
  
  // Update metadata
  x.updateMetadata();
  y.updateMetadata();
  
  return y;
}

// Balance a node using AVL rotations
function balanceNode(node: RopeNode): RopeNode {
  // Update height first
  node.updateMetadata();
  
  const balance = node.getBalance();
  
  // Left heavy
  if (balance > 1) {
    const left = node.getLeft()!;
    
    // Left-Right case
    if (left.getBalance() < 0) {
      node.setLeft(rotateLeft(left));
    }
    
    // Left-Left case
    return rotateRight(node);
  }
  
  // Right heavy
  if (balance < -1) {
    const right = node.getRight()!;
    
    // Right-Left case
    if (right.getBalance() > 0) {
      node.setRight(rotateRight(right));
    }
    
    // Right-Right case
    return rotateLeft(node);
  }
  
  return node; // No balancing needed
}

// Split a leaf node at the given position
function splitLeafNode(node: RopeNode, position: u32): SplitResult {
  if (!node.isLeaf() || position >= node.length) {
    return new SplitResult(node, null); // Cannot split or position out of bounds
  }
  
  if (position == 0) {
    return new SplitResult(null, node); // No split needed
  }
  
  // Read the original text
  const originalLength = node.length;
  let originalText = "";
  
  for (let i: u32 = 0; i < originalLength; i++) {
    const charCode = load<u16>(node.text + i * 2);
    originalText += String.fromCharCode(charCode);
  }
  
  // Create two new nodes with split text
  const leftText = originalText.substr(0, position as i32);
  const rightText = originalText.substr(position as i32);
  
  const leftNode = createLeafNode(leftText);
  const rightNode = createLeafNode(rightText);
  
  // Free the original node
  freeNode(node);
  
  return new SplitResult(leftNode, rightNode);
}

// Concatenate two nodes, maintaining balance
function concat(left: RopeNode | null, right: RopeNode | null): RopeNode | null {
  if (!left) return right;
  if (!right) return left;
  
  // Check if we can merge two small leaf nodes
  if (left.isLeaf() && right.isLeaf()) {
    const totalLength = left.length + right.length;
    
    if (totalLength <= MAX_CHUNK_SIZE) {
      // Merge into single leaf node
      let leftText = "";
      let rightText = "";
      
      // Read left text
      for (let i: u32 = 0; i < left.length; i++) {
        const charCode = load<u16>(left.text + i * 2);
        leftText += String.fromCharCode(charCode);
      }
      
      // Read right text
      for (let i: u32 = 0; i < right.length; i++) {
        const charCode = load<u16>(right.text + i * 2);
        rightText += String.fromCharCode(charCode);
      }
      
      const mergedNode = createLeafNode(leftText + rightText);
      
      // Free original nodes
      freeNode(left);
      freeNode(right);
      
      return mergedNode;
    }
  }
  
  // Create internal node
  const result = createInternalNode(left, right);
  return balanceNode(result);
}

// Global rope root
let ropeRoot: RopeNode | null = null;

// Initialize with empty rope
export function initRope(): void {
  ropeRoot = null;
}

// Split rope at position, returns SplitResult with left and right parts
function splitRope(node: RopeNode | null, position: u32): SplitResult {
  if (!node) return new SplitResult(null, null);
  
  if (node.isLeaf()) {
    if (position >= node.length) {
      return new SplitResult(node, null);
    }
    if (position == 0) {
      return new SplitResult(null, node);
    }
    
    return splitLeafNode(node, position);
  }
  
  // Internal node
  const leftWeight = node.weight;
  
  if (position <= leftWeight) {
    // Split is in left subtree
    const leftSplit = splitRope(node.getLeft(), position);
    const newRight = concat(leftSplit.right, node.getRight());
    return new SplitResult(leftSplit.left, newRight);
  } else {
    // Split is in right subtree
    const rightSplit = splitRope(node.getRight(), position - leftWeight);
    const newLeft = concat(node.getLeft(), rightSplit.left);
    return new SplitResult(newLeft, rightSplit.right);
  }
}

// Insert text at the given position
export function insertText(position: u32, text: string): void {
  if (text.length == 0) return;
  
  const newNode = createLeafNode(text);
  if (changetype<usize>(newNode) == 0) return; // Failed to create node
  
  if (!ropeRoot) {
    ropeRoot = newNode;
    return;
  }
  
  // Clamp position to valid range
  const actualPosition = min(position, ropeRoot!.length);
  
  // Split the rope at the insertion point
  const split = splitRope(ropeRoot, actualPosition);
  
  // Concatenate: left + newNode + right
  let result = concat(split.left, newNode);
  result = concat(result, split.right);
  
  ropeRoot = result;
}

// Delete text in the given range
export function deleteText(start: u32, length: u32): void {
  if (!ropeRoot || length == 0 || start >= ropeRoot!.length) return;
  
  const end = min(start + length, ropeRoot!.length);
  const actualLength = end - start;
  
  if (actualLength == 0) return;
  
  // Split at start position
  const firstSplit = splitRope(ropeRoot, start);
  
  // Split the middle part at the end position
  const secondSplit = splitRope(firstSplit.right, actualLength);
  
  // Free the deleted part
  if (secondSplit.left) {
    freeRopeTree(secondSplit.left!);
  }
  
  // Concatenate left + right
  ropeRoot = concat(firstSplit.left, secondSplit.right);
}

// Recursively free an entire rope tree
function freeRopeTree(node: RopeNode): void {
  if (node.isLeaf()) {
    freeNode(node);
  } else {
    const left = node.getLeft();
    const right = node.getRight();
    
    if (left) freeRopeTree(left);
    if (right) freeRopeTree(right);
    
    freeNode(node);
  }
}

// Get the total length of the rope
export function getRopeLength(): u32 {
  return ropeRoot ? ropeRoot!.length : 0;
}

// Get text from position with given length
export function getRopeText(start: u32, length: u32): string {
  if (!ropeRoot || start >= ropeRoot!.length) {
    return "";
  }
  
  // Simple implementation - build full text then substring
  return getAllRopeText().substr(start as i32, length as i32);
}

// Get all text from the rope (simplified implementation)
export function getAllRopeText(): string {
  if (!ropeRoot) return "";
  
  // For now, implement simple traversal for single leaf
  // This is a minimal implementation for testing
  if (ropeRoot!.isLeaf()) {
    let result = "";
    for (let i: u32 = 0; i < ropeRoot!.length; i++) {
      const charCode = load<u16>(ropeRoot!.text + i * 2);
      result += String.fromCharCode(charCode);
    }
    return result;
  }
  
  // For internal nodes, return a placeholder for now
  return "[Complex rope structure - getText not fully implemented]";
}
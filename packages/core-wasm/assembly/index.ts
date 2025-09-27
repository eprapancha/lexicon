// Core WebAssembly module for Lexicon editor
// High-performance text engine with custom memory management and Rope data structure

import { initMemoryManager, allocBlock, freeBlock } from "./memory";
import { initRope, getAllRopeText, getRopeLength, getTextRange, getTextStats, getCharAt } from "./rope";

export function init(): void {
  // Initialize the memory management system
  initMemoryManager();
  
  // Initialize the rope data structure
  initRope();
}

// Re-export memory management functions for testing
export { 
  allocBlock, 
  freeBlock, 
  allocRopeNode, 
  freeRopeNode 
} from "./memory";

// Re-export rope functions for testing and compatibility
export {
  insertText,
  deleteText,
  getRopeLength,
  getAllRopeText,
  getRopeText,
  getTextRange,
  getTextStats,
  getCharAt
} from "./rope";

// Re-export transaction system (new primary interface)
export {
  applyTransaction,
  getLastResult,
  getLastErrorMessage,
  ErrorCode,
  TransactionType
} from "./transactions";

// Legacy compatibility functions (deprecated - use applyTransaction instead)
export function insert(pos: i32, text: string): void {
  const transaction = `{"type": 0, "position": ${pos}, "text": "${text}"}`;
  applyTransaction(transaction);
}

export function delete(start: i32, end: i32): void {
  const length = end - start;
  if (length > 0) {
    const transaction = `{"type": 1, "position": ${start}, "length": ${length}}`;
    applyTransaction(transaction);
  }
}

export function getText(): string {
  return getAllRopeText();
}

export function getLength(): i32 {
  return getRopeLength() as i32;
}

// String buffer for safe text export
let stringBuffer: usize = 0;
let stringBufferSize: u32 = 0;

// Allocate string buffer for text export
function ensureStringBuffer(size: u32): void {
  if (stringBuffer == 0 || stringBufferSize < size) {
    if (stringBuffer != 0) {
      freeBlock(stringBuffer, stringBufferSize);
    }
    stringBufferSize = size + 1024; // Add some padding
    stringBuffer = allocBlock(stringBufferSize);
  }
}

// Get text range with proper string export
export function getTextInRange(start: i32, end: i32): string {
  const text = getTextRange(start as u32, end as u32);
  
  // Ensure we have a buffer large enough
  const textByteSize = text.length * 2 + 4; // UTF-16 + null terminator
  ensureStringBuffer(textByteSize as u32);
  
  if (stringBuffer == 0) return "";
  
  // Copy string to buffer
  for (let i = 0; i < text.length; i++) {
    store<u16>(stringBuffer + i * 2, text.charCodeAt(i) as u16);
  }
  // Add null terminator
  store<u16>(stringBuffer + text.length * 2, 0);
  
  return text; // Return the string directly - AssemblyScript should handle this
}

// Export buffer pointer for manual reading if needed
export function getStringBufferPtr(): usize {
  return stringBuffer;
}

export function getStringBufferSize(): u32 {
  return stringBufferSize;
}

export function getDocumentStats(): string {
  return getTextStats();
}

export function getCharacterAt(position: i32): i32 {
  return getCharAt(position as u32) as i32;
}

// Import transaction functions
import { 
  applyTransaction,
  applySimpleInsert,
  applyHardcodedInsert,
  getLastResult,
  getLastErrorMessage,
  ErrorCode,
  TransactionType
} from "./transactions";

// Export transaction functions
export { applyTransaction, applySimpleInsert, applyHardcodedInsert, getLastResult, getLastErrorMessage };
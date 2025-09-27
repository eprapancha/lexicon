// Core WebAssembly module for Lexicon editor
// High-performance text engine with custom memory management and Rope data structure

import { initMemoryManager } from "./memory";
import { 
  initRope, 
  insertText, 
  deleteText, 
  getRopeLength, 
  getAllRopeText,
  getRopeText 
} from "./rope";

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

// Re-export rope functions for testing
export {
  insertText,
  deleteText,
  getRopeLength,
  getAllRopeText,
  getRopeText
} from "./rope";

export function insert(pos: i32, text: string): void {
  insertText(pos as u32, text);
}

export function delete(start: i32, end: i32): void {
  const length = end - start;
  if (length > 0) {
    deleteText(start as u32, length as u32);
  }
}

export function getText(): string {
  return getAllRopeText();
}

export function getLength(): i32 {
  return getRopeLength() as i32;
}
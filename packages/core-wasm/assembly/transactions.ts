// Transaction system for atomic operations with comprehensive error handling

import { allocBlock, freeBlock } from "./memory";
import { insertText, deleteText, getRopeLength } from "./rope";

// Error codes for operation results
export enum ErrorCode {
  SUCCESS = 0,
  OUT_OF_MEMORY = 1,
  INVALID_POSITION = 2,
  INVALID_LENGTH = 3,
  INVALID_UTF16 = 4,
  ROPE_CORRUPTION = 5,
  INVALID_TRANSACTION = 6,
  BUFFER_OVERFLOW = 7,
  INVALID_JSON = 8,
  INVALID_PARAMETER = 9
}

// Transaction types
export enum TransactionType {
  INSERT = 0,
  DELETE = 1,
  REPLACE = 2,
  COMPOUND = 3
}

// Transaction operation structure
@unmanaged
class TransactionOp {
  type: u32;      // TransactionType
  position: u32;  // Character position
  length: u32;    // Length for delete/replace operations
  textPtr: usize; // Pointer to text data
  textLength: u32; // Length of text data
  
  constructor() {
    this.type = TransactionType.INSERT;
    this.position = 0;
    this.length = 0;
    this.textPtr = 0;
    this.textLength = 0;
  }
}

// Transaction result structure
@unmanaged
class TransactionResult {
  errorCode: u32;
  newCursorPosition: u32;
  newLength: u32;
  errorMessagePtr: usize;
  errorMessageLength: u32;
  
  constructor() {
    this.errorCode = ErrorCode.SUCCESS;
    this.newCursorPosition = 0;
    this.newLength = 0;
    this.errorMessagePtr = 0;
    this.errorMessageLength = 0;
  }
}

// Global variables for transaction state
let lastResult: TransactionResult = new TransactionResult();
let lastErrorMessage: string = "";

// Allocate and store error message
function setErrorMessage(message: string): void {
  lastErrorMessage = message;
  // Free previous error message if exists
  if (lastResult.errorMessagePtr != 0) {
    freeBlock(lastResult.errorMessagePtr, lastResult.errorMessageLength);
  }
  
  // Allocate new error message
  const messageLength = message.length * 2; // UTF-16
  const messagePtr = allocBlock(messageLength as u32 + 4); // Add padding
  
  if (messagePtr != 0) {
    // Copy error message to allocated memory
    for (let i = 0; i < message.length; i++) {
      store<u16>(messagePtr + i * 2, message.charCodeAt(i) as u16);
    }
    
    lastResult.errorMessagePtr = messagePtr;
    lastResult.errorMessageLength = messageLength as u32;
  }
}

// Validate transaction operation
function validateTransactionOp(op: TransactionOp): ErrorCode {
  const currentLength = getRopeLength();
  
  switch (op.type) {
    case TransactionType.INSERT:
      if (op.position > currentLength) {
        setErrorMessage(`Invalid insert position ${op.position}, max allowed: ${currentLength}`);
        return ErrorCode.INVALID_POSITION;
      }
      if (op.textPtr == 0 && op.textLength > 0) {
        setErrorMessage("Text pointer is null but text length > 0");
        return ErrorCode.INVALID_UTF16;
      }
      break;
    
    case TransactionType.DELETE:
      if (op.position >= currentLength) {
        return ErrorCode.INVALID_POSITION;
      }
      if (op.length == 0) {
        return ErrorCode.INVALID_LENGTH;
      }
      if (op.position + op.length > currentLength) {
        return ErrorCode.INVALID_LENGTH;
      }
      break;
    
    case TransactionType.REPLACE:
      if (op.position >= currentLength) {
        return ErrorCode.INVALID_POSITION;
      }
      if (op.length == 0) {
        return ErrorCode.INVALID_LENGTH;
      }
      if (op.position + op.length > currentLength) {
        return ErrorCode.INVALID_LENGTH;
      }
      if (op.textPtr == 0 && op.textLength > 0) {
        return ErrorCode.INVALID_UTF16;
      }
      break;
    
    default:
      return ErrorCode.INVALID_TRANSACTION;
  }
  
  return ErrorCode.SUCCESS;
}

// Apply a single transaction operation
function applySingleOp(op: TransactionOp): ErrorCode {
  // Quick sanity check on the parsed operation
  if (op.type > 3) { // We only have INSERT(0), DELETE(1), REPLACE(2), COMPOUND(3)
    setErrorMessage(`Invalid transaction type: ${op.type}`);
    return ErrorCode.INVALID_TRANSACTION;
  }
  
  // Validate operation first
  const validationResult = validateTransactionOp(op);
  if (validationResult != ErrorCode.SUCCESS) {
    return validationResult;
  }
  
  // Convert text pointer to string if needed
  let text = "";
  if (op.textPtr != 0 && op.textLength > 0) {
    for (let i: u32 = 0; i < op.textLength / 2; i++) {
      const charCode = load<u16>(op.textPtr + i * 2);
      text += String.fromCharCode(charCode);
    }
  }
  
  // Apply the operation
  switch (op.type) {
    case TransactionType.INSERT:
      insertText(op.position, text);
      lastResult.newCursorPosition = op.position + text.length as u32;
      break;
    
    case TransactionType.DELETE:
      deleteText(op.position, op.length);
      lastResult.newCursorPosition = op.position;
      break;
    
    case TransactionType.REPLACE:
      // Replace = delete + insert
      deleteText(op.position, op.length);
      insertText(op.position, text);
      lastResult.newCursorPosition = op.position + text.length as u32;
      break;
    
    default:
      return ErrorCode.INVALID_TRANSACTION;
  }
  
  lastResult.newLength = getRopeLength();
  return ErrorCode.SUCCESS;
}

// Parse simple JSON-like transaction format
// Format: {"type": 0, "position": 10, "text": "hello"}
function parseTransactionJson(jsonStr: string): TransactionOp | null {
  // Simplified parser - extract values by character scanning
  
  if (jsonStr.length == 0) return null;
  
  const op = new TransactionOp();
  let pos = 0;
  
  // Look for "type": value
  while (pos < jsonStr.length - 6) {
    if (jsonStr.charAt(pos) == 't' && 
        jsonStr.charAt(pos+1) == 'y' &&
        jsonStr.charAt(pos+2) == 'p' &&
        jsonStr.charAt(pos+3) == 'e') {
      // Found "type", look for the value
      pos += 4;
      while (pos < jsonStr.length && jsonStr.charAt(pos) != ':') pos++;
      pos++; // Skip ':'
      while (pos < jsonStr.length && (jsonStr.charAt(pos) == ' ' || jsonStr.charAt(pos) == '\t')) pos++;
      
      // Parse number
      let num = 0;
      while (pos < jsonStr.length) {
        const ch = jsonStr.charAt(pos);
        if (ch >= '0' && ch <= '9') {
          num = num * 10 + (ch.charCodeAt(0) - 48);
          pos++;
        } else {
          break;
        }
      }
      op.type = num as u32;
      break;
    }
    pos++;
  }
  
  // Look for "position": value
  pos = 0;
  while (pos < jsonStr.length - 10) {
    if (jsonStr.charAt(pos) == 'p' && 
        jsonStr.charAt(pos+1) == 'o' &&
        jsonStr.charAt(pos+2) == 's' &&
        jsonStr.charAt(pos+3) == 'i' &&
        jsonStr.charAt(pos+4) == 't' &&
        jsonStr.charAt(pos+5) == 'i' &&
        jsonStr.charAt(pos+6) == 'o' &&
        jsonStr.charAt(pos+7) == 'n') {
      // Found "position", look for the value
      pos += 8;
      while (pos < jsonStr.length && jsonStr.charAt(pos) != ':') pos++;
      pos++; // Skip ':'
      while (pos < jsonStr.length && (jsonStr.charAt(pos) == ' ' || jsonStr.charAt(pos) == '\t')) pos++;
      
      // Parse number
      let num = 0;
      while (pos < jsonStr.length) {
        const ch = jsonStr.charAt(pos);
        if (ch >= '0' && ch <= '9') {
          num = num * 10 + (ch.charCodeAt(0) - 48);
          pos++;
        } else {
          break;
        }
      }
      op.position = num as u32;
      break;
    }
    pos++;
  }
  
  // Look for "text": "value" (for insert/replace operations)
  pos = 0;
  while (pos < jsonStr.length - 6) {
    if (jsonStr.charAt(pos) == 't' && 
        jsonStr.charAt(pos+1) == 'e' &&
        jsonStr.charAt(pos+2) == 'x' &&
        jsonStr.charAt(pos+3) == 't') {
      // Found "text", look for the value
      pos += 4;
      while (pos < jsonStr.length && jsonStr.charAt(pos) != ':') pos++;
      pos++; // Skip ':'
      while (pos < jsonStr.length && (jsonStr.charAt(pos) == ' ' || jsonStr.charAt(pos) == '\t')) pos++;
      
      if (pos < jsonStr.length && jsonStr.charAt(pos) == '"') {
        pos++; // Skip opening quote
        const textStart = pos;
        
        // Find closing quote
        while (pos < jsonStr.length && jsonStr.charAt(pos) != '"') pos++;
        const textEnd = pos;
        
        if (textEnd > textStart) {
          const textLength = textEnd - textStart;
          const memSize = textLength * 2 + 4; // UTF-16 + padding
          const textPtr = allocBlock(memSize as u32);
          
          if (textPtr != 0) {
            // Copy text to allocated memory
            for (let i = 0; i < textLength; i++) {
              const ch = jsonStr.charCodeAt(textStart + i);
              store<u16>(textPtr + i * 2, ch as u16);
            }
            
            op.textPtr = textPtr;
            op.textLength = (textLength * 2) as u32;
          } else {
            return null; // Out of memory
          }
        }
      }
      break;
    }
    pos++;
  }
  
  // Look for "length": value (for delete/replace operations)
  pos = 0;
  while (pos < jsonStr.length - 8) {
    if (jsonStr.charAt(pos) == 'l' && 
        jsonStr.charAt(pos+1) == 'e' &&
        jsonStr.charAt(pos+2) == 'n' &&
        jsonStr.charAt(pos+3) == 'g' &&
        jsonStr.charAt(pos+4) == 't' &&
        jsonStr.charAt(pos+5) == 'h') {
      // Found "length", look for the value
      pos += 6;
      while (pos < jsonStr.length && jsonStr.charAt(pos) != ':') pos++;
      pos++; // Skip ':'
      while (pos < jsonStr.length && (jsonStr.charAt(pos) == ' ' || jsonStr.charAt(pos) == '\t')) pos++;
      
      // Parse number
      let num = 0;
      while (pos < jsonStr.length) {
        const ch = jsonStr.charAt(pos);
        if (ch >= '0' && ch <= '9') {
          num = num * 10 + (ch.charCodeAt(0) - 48);
          pos++;
        } else {
          break;
        }
      }
      op.length = num as u32;
      break;
    }
    pos++;
  }
  
  return op;
}

// Test function with hardcoded text for debugging
export function applyHardcodedInsert(position: u32): u32 {
  const text = "Hello"; // Hardcoded text to avoid string parameter issues
  
  // Reset result state
  lastResult.errorCode = ErrorCode.SUCCESS;
  lastResult.newCursorPosition = 0;
  lastResult.newLength = getRopeLength();
  
  // Use direct rope insertion for testing
  insertText(position, text);
  
  lastResult.newCursorPosition = position + text.length as u32;
  lastResult.newLength = getRopeLength();
  
  return ErrorCode.SUCCESS;
}

// Test function that bypasses JSON parsing for debugging
export function applySimpleInsert(position: u32, text: string): u32 {
  if (!text || text.length == 0) {
    return ErrorCode.INVALID_PARAMETER;
  }
  
  // Reset result state
  lastResult.errorCode = ErrorCode.SUCCESS;
  lastResult.newCursorPosition = 0;
  lastResult.newLength = getRopeLength();
  
  // Create a simple insert operation manually
  const op = new TransactionOp();
  op.type = TransactionType.INSERT;
  op.position = position;
  op.length = 0;
  
  // Allocate text manually 
  const textLength = text.length * 2;
  const textPtr = allocBlock(textLength as u32 + 4);
  
  if (textPtr == 0) {
    return ErrorCode.OUT_OF_MEMORY;
  }
  
  // Copy text to allocated memory
  for (let i = 0; i < text.length; i++) {
    store<u16>(textPtr + i * 2, text.charCodeAt(i) as u16);
  }
  
  op.textPtr = textPtr;
  op.textLength = textLength as u32;
  
  // Apply operation
  const result = applySingleOp(op);
  lastResult.errorCode = result;
  
  // Free the allocated text memory
  freeBlock(textPtr, textLength as u32 + 4);
  
  return result;
}

// Main transaction application function
export function applyTransaction(transactionJson: string): u32 {
  // Initialize result
  if (!lastResult) {
    lastResult = new TransactionResult();
  } else {
    // Clear previous error message
    if (lastResult.errorMessagePtr != 0) {
      freeBlock(lastResult.errorMessagePtr, lastResult.errorMessageLength);
      lastResult.errorMessagePtr = 0;
      lastResult.errorMessageLength = 0;
    }
  }
  
  lastResult.errorCode = ErrorCode.SUCCESS;
  lastResult.newCursorPosition = 0;
  lastResult.newLength = getRopeLength();
  
  // Parse transaction
  const op = parseTransactionJson(transactionJson);
  if (!op) {
    lastResult.errorCode = ErrorCode.INVALID_JSON;
    setErrorMessage("Failed to parse transaction JSON");
    return ErrorCode.INVALID_JSON;
  }
  
  // Debug: Check if op is valid
  if (changetype<usize>(op) == 0) {
    lastResult.errorCode = ErrorCode.INVALID_JSON;
    setErrorMessage("Transaction operation is null");
    return ErrorCode.INVALID_JSON;
  }
  
  // Apply operation
  const result = applySingleOp(op);
  lastResult.errorCode = result;
  
  // Set appropriate error message
  switch (result) {
    case ErrorCode.SUCCESS:
      break;
    case ErrorCode.OUT_OF_MEMORY:
      setErrorMessage("Insufficient memory for operation");
      break;
    case ErrorCode.INVALID_POSITION:
      setErrorMessage("Invalid cursor position");
      break;
    case ErrorCode.INVALID_LENGTH:
      setErrorMessage("Invalid length parameter");
      break;
    case ErrorCode.INVALID_UTF16:
      setErrorMessage("Invalid UTF-16 text data");
      break;
    case ErrorCode.ROPE_CORRUPTION:
      setErrorMessage("Internal rope data structure error");
      break;
    case ErrorCode.INVALID_TRANSACTION:
      setErrorMessage("Invalid transaction type");
      break;
    case ErrorCode.INVALID_JSON:
      setErrorMessage("Invalid JSON format");
      break;
    default:
      setErrorMessage("Unknown error occurred");
      break;
  }
  
  // Free operation text memory
  if (op.textPtr != 0) {
    freeBlock(op.textPtr, op.textLength + 4);
  }
  
  return result;
}

// Get last transaction result
export function getLastResult(): string {
  if (!lastResult) {
    return `{"errorCode": ${ErrorCode.SUCCESS}, "cursorPosition": 0, "length": 0}`;
  }
  
  return `{"errorCode": ${lastResult.errorCode}, "cursorPosition": ${lastResult.newCursorPosition}, "length": ${lastResult.newLength}}`;
}

// Get last error message
export function getLastErrorMessage(): string {
  return lastErrorMessage;
}

// Import rope functions
import { 
  insertText, 
  deleteText, 
  getRopeLength, 
  getAllRopeText,
  getRopeText,
  initRope 
} from "./rope";

import {
  allocBlock,
  freeBlock
} from "./memory";
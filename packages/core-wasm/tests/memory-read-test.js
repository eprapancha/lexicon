// Test reading strings from WASM linear memory

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Helper function to read UTF-16 string from WASM memory
function readStringFromWASM(wasmInstance, ptr, maxLength = 1000) {
  if (ptr === 0) return "";
  
  const memory = wasmInstance.exports.memory;
  const buffer = new Uint8Array(memory.buffer);
  const uint16Array = new Uint16Array(memory.buffer);
  
  // Calculate 16-bit offset
  const uint16Offset = ptr / 2;
  
  let str = "";
  let i = 0;
  
  // Read UTF-16 characters until null terminator or max length
  while (i < maxLength) {
    const charCode = uint16Array[uint16Offset + i];
    if (charCode === 0) break; // Null terminator
    str += String.fromCharCode(charCode);
    i++;
  }
  
  return str;
}

// Alternative: read string with known length
function readStringWithLength(wasmInstance, ptr, length) {
  if (ptr === 0 || length === 0) return "";
  
  const memory = wasmInstance.exports.memory;
  const uint16Array = new Uint16Array(memory.buffer);
  const uint16Offset = ptr / 2;
  
  let str = "";
  for (let i = 0; i < length; i++) {
    const charCode = uint16Array[uint16Offset + i];
    str += String.fromCharCode(charCode);
  }
  
  return str;
}

async function memoryReadTest() {
  try {
    console.log('🔧 WASM Memory String Reading Test');
    
    const wasmPath = path.join(__dirname, '../build/debug.wasm');
    const wasmBuffer = fs.readFileSync(wasmPath);
    const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
      env: {
        abort: () => {
          throw new Error('WebAssembly aborted');
        }
      }
    });
    
    const wasmInstance = wasmModule.instance;
    const { exports } = wasmInstance;
    
    // Initialize
    exports.init();
    console.log('✅ WASM initialized');
    
    // Insert text
    const errorCode = exports.applyHardcodedInsert(0);
    console.log('Insert result:', errorCode, '(0 = success)');
    console.log('Text length:', exports.getLength());
    
    // Test memory-based string reading
    console.log('\n🔧 Testing memory-based string reading...');
    
    // Get the pointer returned by getTextInRange
    const ptr = exports.getTextInRange(0, 5);
    console.log('Returned pointer:', ptr);
    
    if (ptr !== 0) {
      // Try reading from memory at this pointer
      const stringFromMemory = readStringFromWASM(wasmInstance, ptr, 10);
      console.log('String read from memory:', JSON.stringify(stringFromMemory));
      
      // Also try reading with known length
      const stringWithLength = readStringWithLength(wasmInstance, ptr, 5);
      console.log('String with known length:', JSON.stringify(stringWithLength));
    }
    
    // Test buffer-based approach
    console.log('\n🔧 Testing string buffer approach...');
    const bufferPtr = exports.getStringBufferPtr();
    console.log('String buffer pointer:', bufferPtr);
    
    if (bufferPtr !== 0) {
      const bufferString = readStringFromWASM(wasmInstance, bufferPtr, 20);
      console.log('String from buffer:', JSON.stringify(bufferString));
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

memoryReadTest();
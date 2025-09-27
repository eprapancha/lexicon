// Debug memory allocation during transactions

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Safe transaction helper
function applyTransactionSafe(wasmInstance, jsonString) {
  const exports = wasmInstance.exports;
  
  console.log(`    Applying transaction: ${jsonString}`);
  
  // Allocate memory in WASM for the string
  const stringLength = jsonString.length;
  console.log(`    String length: ${stringLength}`);
  
  const ptr = exports.allocateString(stringLength);
  console.log(`    Allocated pointer: ${ptr}`);
  
  if (ptr === 0) {
    console.log('    ❌ Memory allocation failed');
    return 1; // OUT_OF_MEMORY
  }
  
  // Copy string to WASM memory
  for (let i = 0; i < stringLength; i++) {
    exports.setStringChar(ptr, i, jsonString.charCodeAt(i));
  }
  console.log(`    ✅ String copied to WASM memory`);
  
  // Call the safe transaction function
  try {
    const result = exports.applyTransactionFromPtr(ptr, stringLength);
    console.log(`    Transaction result: ${result}`);
    return result;
  } catch (e) {
    console.log(`    ❌ Transaction failed: ${e.message}`);
    return 99; // Unknown error
  } finally {
    // Free the allocated memory
    try {
      exports.freeBlock(ptr, stringLength * 2 + 4);
      console.log(`    ✅ Memory freed`);
    } catch (e) {
      console.log(`    ⚠️ Memory free failed: ${e.message}`);
    }
  }
}

async function memoryDebugTest() {
  try {
    console.log('🔍 Memory Allocation Debug Test');
    console.log('===============================');
    
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
    console.log('Initial length:', exports.getLength());
    
    // Test 1: First transaction (this worked before)
    console.log('\\n🧪 Test 1: First Transaction');
    let result = applyTransactionSafe(wasmInstance, '{"type":0,"position":0,"text":"Hi"}');
    console.log('Result:', result === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after:', exports.getLength());
    
    // Test 2: Second transaction (this failed before)
    console.log('\\n🧪 Test 2: Second Transaction');
    result = applyTransactionSafe(wasmInstance, '{"type":0,"position":2,"text":"!"}');
    console.log('Result:', result === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after:', exports.getLength());
    
    // Test 3: Third transaction to see if pattern continues
    console.log('\\n🧪 Test 3: Third Transaction');
    result = applyTransactionSafe(wasmInstance, '{"type":0,"position":3,"text":"?"}');
    console.log('Result:', result === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after:', exports.getLength());
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

memoryDebugTest();
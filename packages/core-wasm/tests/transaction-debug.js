// Debug transaction parsing and validation

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Helper to get error messages
function getErrorMessage(exports) {
  try {
    return exports.getLastErrorMessage();
  } catch (e) {
    return "Could not get error message";
  }
}

// Safe transaction helper with detailed error reporting
function applyTransactionWithDebug(wasmInstance, jsonString) {
  const exports = wasmInstance.exports;
  
  console.log(`  📋 Transaction: ${jsonString}`);
  
  const stringLength = jsonString.length;
  const ptr = exports.allocateString(stringLength);
  
  if (ptr === 0) {
    console.log('  ❌ Memory allocation failed');
    return 1;
  }
  
  // Copy string to WASM memory
  for (let i = 0; i < stringLength; i++) {
    exports.setStringChar(ptr, i, jsonString.charCodeAt(i));
  }
  
  const currentLength = exports.getLength();
  console.log(`  📏 Current text length: ${currentLength}`);
  
  try {
    const result = exports.applyTransactionFromPtr(ptr, stringLength);
    console.log(`  🎯 Transaction result: ${result}`);
    
    if (result !== 0) {
      const errorMsg = getErrorMessage(exports);
      console.log(`  💬 Error message: "${errorMsg}"`);
    }
    
    return result;
  } catch (e) {
    console.log(`  💥 Exception: ${e.message}`);
    return 99;
  } finally {
    try {
      exports.freeBlock(ptr, stringLength * 2 + 4);
    } catch (e) {
      console.log(`  ⚠️ Memory free failed: ${e.message}`);
    }
  }
}

async function transactionDebugTest() {
  try {
    console.log('🔬 Transaction Debug Test');
    console.log('=========================');
    
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
    
    // Test 1: Insert "Hi" at position 0
    console.log('\\n🧪 Test 1: Insert "Hi" at position 0');
    let result = applyTransactionWithDebug(wasmInstance, '{"type":0,"position":0,"text":"Hi"}');
    console.log(`Result: ${result === 0 ? '✅ Success' : '❌ Failed'}`);
    console.log(`New length: ${exports.getLength()}`);
    
    // Test 2: Insert "!" at position 2 (should be valid - after "Hi")
    console.log('\\n🧪 Test 2: Insert "!" at position 2');
    result = applyTransactionWithDebug(wasmInstance, '{"type":0,"position":2,"text":"!"}');
    console.log(`Result: ${result === 0 ? '✅ Success' : '❌ Failed'}`);
    console.log(`New length: ${exports.getLength()}`);
    
    // Test 3: Insert "?" at position 1 (middle of "Hi!")
    console.log('\\n🧪 Test 3: Insert "?" at position 1');
    result = applyTransactionWithDebug(wasmInstance, '{"type":0,"position":1,"text":"?"}');
    console.log(`Result: ${result === 0 ? '✅ Success' : '❌ Failed'}`);
    console.log(`New length: ${exports.getLength()}`);
    
    // Show final text character by character
    console.log('\\n📝 Final text:');
    const finalLength = exports.getLength();
    for (let i = 0; i < finalLength; i++) {
      const char = String.fromCharCode(exports.getCharacterAt(i));
      console.log(`  ${i}: '${char}'`);
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

transactionDebugTest();
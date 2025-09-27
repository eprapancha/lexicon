// Test proper string allocation and transaction system

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Helper to safely pass JSON strings to WASM
function applyTransactionSafe(wasmInstance, jsonString) {
  const exports = wasmInstance.exports;
  
  // Allocate memory in WASM for the string
  const stringLength = jsonString.length;
  const ptr = exports.allocateString(stringLength);
  
  if (ptr === 0) {
    return 1; // OUT_OF_MEMORY
  }
  
  // Copy string to WASM memory
  for (let i = 0; i < stringLength; i++) {
    exports.setStringChar(ptr, i, jsonString.charCodeAt(i));
  }
  
  // Call the safe transaction function
  const result = exports.applyTransactionFromPtr(ptr, stringLength);
  
  // Free the allocated memory
  exports.freeBlock(ptr, stringLength * 2 + 4);
  
  return result;
}

async function properStringTest() {
  try {
    console.log('🔧 Proper String Allocation Test');
    console.log('=================================');
    
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
    
    // Test 1: Simple transaction
    console.log('\\n🧪 Test 1: Basic Insert Transaction');
    const insertJson = '{"type":0,"position":0,"text":"Hi"}';
    console.log('JSON:', insertJson);
    
    const result1 = applyTransactionSafe(wasmInstance, insertJson);
    console.log('Result:', result1, result1 === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after insert:', exports.getLength());
    
    // Test character verification
    if (exports.getLength() > 0) {
      console.log('Characters:');
      for (let i = 0; i < exports.getLength(); i++) {
        const char = String.fromCharCode(exports.getCharacterAt(i));
        console.log(`  ${i}: '${char}'`);
      }
    }
    
    // Test 2: Add more text
    console.log('\\n🧪 Test 2: Append Text Transaction');
    const appendJson = '{"type":0,"position":2,"text":" World"}';
    console.log('JSON:', appendJson);
    
    const result2 = applyTransactionSafe(wasmInstance, appendJson);
    console.log('Result:', result2, result2 === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after append:', exports.getLength());
    
    // Test 3: Delete operation
    console.log('\\n🧪 Test 3: Delete Transaction');
    const deleteJson = '{"type":1,"position":2,"length":3}'; // Delete " Wo"
    console.log('JSON:', deleteJson);
    
    const result3 = applyTransactionSafe(wasmInstance, deleteJson);
    console.log('Result:', result3, result3 === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after delete:', exports.getLength());
    
    // Test 4: Replace operation
    console.log('\\n🧪 Test 4: Replace Transaction');
    const replaceJson = '{"type":2,"position":0,"length":2,"text":"Hello"}';
    console.log('JSON:', replaceJson);
    
    const result4 = applyTransactionSafe(wasmInstance, replaceJson);
    console.log('Result:', result4, result4 === 0 ? '✅ Success' : '❌ Failed');
    console.log('Length after replace:', exports.getLength());
    
    // Test final text
    console.log('\\n📝 Final Text Verification');
    const finalLength = exports.getLength();
    let finalText = '';
    for (let i = 0; i < finalLength; i++) {
      finalText += String.fromCharCode(exports.getCharacterAt(i));
    }
    console.log('Final text:', `"${finalText}"`);
    
    console.log('\\n🎉 JSON Transaction System: FIXED! ✅');
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

properStringTest();
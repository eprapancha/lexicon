// Simple test to isolate JSON parsing vs validation issues

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

function applyTransactionSafe(wasmInstance, jsonString) {
  const exports = wasmInstance.exports;
  const stringLength = jsonString.length;
  const ptr = exports.allocateString(stringLength);
  
  if (ptr === 0) return 1;
  
  for (let i = 0; i < stringLength; i++) {
    exports.setStringChar(ptr, i, jsonString.charCodeAt(i));
  }
  
  try {
    const result = exports.applyTransactionFromPtr(ptr, stringLength);
    return result;
  } finally {
    try {
      exports.freeBlock(ptr, stringLength * 2 + 4);
    } catch (e) {}
  }
}

async function simpleJsonTest() {
  try {
    console.log('🔎 Simple JSON Parsing Test');
    
    const wasmPath = path.join(__dirname, '../build/debug.wasm');
    const wasmBuffer = fs.readFileSync(wasmPath);
    const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
      env: {
        abort: () => {
          throw new Error('WebAssembly aborted');
        }
      }
    });
    
    const { exports } = wasmModule.instance;
    
    exports.init();
    console.log('✅ WASM initialized');
    
    // Test different positions that should all be valid for empty text
    const testCases = [
      '{"type":0,"position":0,"text":"A"}',  // Insert at start
    ];
    
    for (let i = 0; i < testCases.length; i++) {
      console.log(`\\nTest ${i + 1}: ${testCases[i]}`);
      console.log(`Current length: ${exports.getLength()}`);
      
      const result = applyTransactionSafe(wasmModule.instance, testCases[i]);
      console.log(`Result: ${result} (${result === 0 ? 'Success' : 'Failed'})`);
      console.log(`New length: ${exports.getLength()}`);
      
      if (result !== 0) {
        console.log('Breaking on first failure to prevent cascading issues');
        break;
      }
    }
    
    // If we got this far, test the problematic case from before
    if (exports.getLength() > 0) {
      console.log('\\n🔬 Testing the problematic insertion...');
      console.log(`Current text length: ${exports.getLength()}`);
      console.log('Attempting to insert at the end position...');
      
      const endPos = exports.getLength();
      const problematicJson = `{"type":0,"position":${endPos},"text":"!"}`;
      console.log(`JSON: ${problematicJson}`);
      
      const result = applyTransactionSafe(wasmModule.instance, problematicJson);
      console.log(`Result: ${result} (${result === 0 ? 'Success' : 'Failed'})`);
      
      // Check what our validation logic should think about this
      console.log(`Position ${endPos} should be valid for length ${exports.getLength()}`);
      console.log(`Validation rule: position <= length means ${endPos} <= ${exports.getLength()} = ${endPos <= exports.getLength()}`);
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

simpleJsonTest();
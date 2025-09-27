// Test sequence of operations to isolate the issue

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function sequenceTest() {
  try {
    console.log('🔄 Sequence Operation Test');
    
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
    
    // Test 1: Use hardcoded insert (known working)
    console.log('\\n🧪 Test 1: First hardcoded insert');
    let result = exports.applyHardcodedInsert(0); // Inserts "Hello"
    console.log(`Result: ${result} (${result === 0 ? 'Success' : 'Failed'})`);
    console.log(`Length: ${exports.getLength()}`);
    
    // Test 2: Use legacy insertText directly
    console.log('\\n🧪 Test 2: Legacy insertText');
    try {
      exports.insertText(5, '!'); // This might fail due to string passing
      console.log('Legacy insert: Success');
      console.log(`Length: ${exports.getLength()}`);
    } catch (e) {
      console.log('Legacy insert: Failed (expected due to string passing)');
    }
    
    // Test 3: Second hardcoded insert 
    console.log('\\n🧪 Test 3: Second hardcoded insert');
    result = exports.applyHardcodedInsert(exports.getLength()); // Insert at end
    console.log(`Result: ${result} (${result === 0 ? 'Success' : 'Failed'})`);
    console.log(`Length: ${exports.getLength()}`);
    
    // Show current text
    console.log('\\n📝 Current text:');
    const currentLength = exports.getLength();
    for (let i = 0; i < currentLength; i++) {
      const char = String.fromCharCode(exports.getCharacterAt(i));
      console.log(`  ${i}: '${char}'`);
    }
    
    console.log('\\n🔍 Analysis:');
    console.log('If hardcoded inserts work but JSON transactions fail,');
    console.log('the issue is in JSON parsing, not the rope data structure.');
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

sequenceTest();
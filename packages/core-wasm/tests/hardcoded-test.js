// Test hardcoded insert function

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function hardcodedTest() {
  try {
    console.log('🔧 Hardcoded Insert Test');
    
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
    
    // Initialize
    exports.init();
    console.log('✅ WASM initialized');
    console.log('Initial length:', exports.getLength());
    
    // Test using the hardcoded insert function
    console.log('\n🔧 Testing applyHardcodedInsert...');
    
    const errorCode = exports.applyHardcodedInsert(0);
    console.log('Insert error code:', errorCode, '(0 = success)');
    
    console.log('New length after insert:', exports.getLength());
    
    // Test character access
    if (exports.getLength() > 0) {
      console.log('First 5 character codes:');
      for (let i = 0; i < Math.min(5, exports.getLength()); i++) {
        const charCode = exports.getCharacterAt(i);
        console.log(`  ${i}: ${charCode} ('${String.fromCharCode(charCode)}')`);
      }
    }
    
    // Try to get text using getTextInRange
    if (exports.getLength() > 0) {
      console.log('\n🔧 Testing text retrieval...');
      try {
        const textResult = exports.getTextInRange(0, Math.min(10, exports.getLength()));
        console.log('Text via getTextInRange:', JSON.stringify(textResult));
      } catch (e) {
        console.log('getTextInRange failed:', e.message);
      }
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

hardcodedTest();
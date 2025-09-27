// Test simplified insert function

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function simpleInsertTest() {
  try {
    console.log('🔧 Simple Insert Test');
    
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
    
    // Test using the simplified insert function that bypasses JSON
    console.log('\n🔧 Testing applySimpleInsert...');
    
    const errorCode = exports.applySimpleInsert(0, 'Hi');
    console.log('Insert error code:', errorCode, '(0 = success)');
    
    console.log('New length after insert:', exports.getLength());
    
    // Test character access
    if (exports.getLength() > 0) {
      console.log('First character code:', exports.getCharacterAt(0));
      console.log('Second character code:', exports.getCharacterAt(1));
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

simpleInsertTest();
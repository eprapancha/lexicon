// Test insert using only internal strings

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function insertTest() {
  try {
    console.log('🔧 Insert Test');
    
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
    console.log('Initial text repr:', JSON.stringify(exports.getText()));
    
    // Try very simple legacy insert (bypassing transactions for now)
    console.log('\n🔧 Testing basic legacy insertText...');
    
    try {
      // Use the simplest possible string
      const result = exports.insertText(0, 'A');
      console.log('Insert result (no return value expected)');
      console.log('New length:', exports.getLength());
      console.log('New text:', JSON.stringify(exports.getText()));
    } catch (insertError) {
      console.log('❌ Insert failed:', insertError);
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

insertTest();
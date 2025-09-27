// Simple test to debug the JSON parsing issue

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function simpleTest() {
  try {
    console.log('🔧 Simple WASM Test');
    
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
    
    // Test basic rope operations first
    exports.insertText(0, "Hello");
    console.log('✅ Basic insert worked');
    console.log('Text:', exports.getText());
    console.log('Length:', exports.getLength());
    
    // Now test simple transaction with minimal JSON
    console.log('\n🧪 Testing simple transaction...');
    const simpleJson = '{"type":0,"position":5,"text":" World"}';
    console.log('Transaction JSON:', simpleJson);
    
    const result = exports.applyTransaction(simpleJson);
    console.log('Result code:', result);
    
    if (result === 0) {
      console.log('✅ Transaction succeeded');
      console.log('New text:', exports.getText());
      console.log('New length:', exports.getLength());
    } else {
      console.log('❌ Transaction failed with code:', result);
      console.log('Error:', exports.getLastErrorMessage());
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

simpleTest();
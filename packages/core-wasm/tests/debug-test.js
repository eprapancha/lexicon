// Debug test using only numbers

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function debugTest() {
  try {
    console.log('🔧 Debug Test - Numbers Only');
    
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
    
    // Test only numeric functions
    console.log('Initial length:', exports.getLength());
    
    // Try a transaction using the transaction system
    console.log('\n🔧 Testing numeric transaction...');
    const transactionJson = '{"type":0,"position":0,"text":"Hi"}';
    console.log('Transaction:', transactionJson);
    
    const errorCode = exports.applyTransaction(transactionJson);
    console.log('Transaction error code:', errorCode, '(0 = success)');
    
    console.log('New length after transaction:', exports.getLength());
    
    // Test character access
    if (exports.getLength() > 0) {
      console.log('First character code:', exports.getCharacterAt(0));
      console.log('Second character code:', exports.getCharacterAt(1));
    }
    
  } catch (error) {
    console.error('❌ Test failed:', error);
  }
}

debugTest();
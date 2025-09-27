// Ultra-basic test to check string handling

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function basicTest() {
  try {
    console.log('🔧 Basic WASM String Test');
    
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
    
    // Test text length after init
    console.log('Initial text length:', exports.getLength());
    console.log('Initial text:', exports.getText());
    
  } catch (error) {
    console.error('❌ Basic test failed:', error);
  }
}

basicTest();
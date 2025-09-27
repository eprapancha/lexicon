// Minimal test to isolate the string issue

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function runMinimalTest() {
  try {
    console.log('Loading WebAssembly module...');
    
    const wasmPath = path.join(__dirname, '../build/debug.wasm');
    
    if (!fs.existsSync(wasmPath)) {
      console.error('WebAssembly module not found. Please run: npm run asbuild:debug');
      process.exit(1);
    }
    
    const wasmBuffer = fs.readFileSync(wasmPath);
    const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
      env: {
        abort: () => {
          throw new Error('WebAssembly aborted');
        }
      }
    });
    
    const { exports } = wasmModule.instance;
    
    // Initialize the system
    exports.init();
    console.log('✅ WebAssembly module initialized');
    
    // Test memory allocation first
    console.log('Testing memory allocation...');
    const ptr1 = exports.allocBlock(32);
    console.log(`Allocated 32 bytes at: ${ptr1}`);
    
    if (ptr1 !== 0) {
      exports.freeBlock(ptr1, 32);
      console.log('Memory freed successfully');
    }
    
    // Test rope node allocation
    console.log('Testing rope node allocation...');
    const node1 = exports.allocRopeNode();
    console.log(`Allocated rope node at: ${node1}`);
    
    if (node1 !== 0) {
      exports.freeRopeNode(node1);
      console.log('Rope node freed successfully');
    }
    
    // Test simple string operations
    console.log('Testing basic operations...');
    console.log(`Initial length: ${exports.getLength()}`);
    console.log(`Initial text: "${exports.getText()}"`);
    
    console.log('✅ Minimal test completed successfully!');
    
  } catch (error) {
    console.error('❌ Minimal test failed:', error);
    process.exit(1);
  }
}

runMinimalTest();
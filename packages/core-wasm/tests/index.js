// Node.js test runner for WebAssembly memory management tests

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function runTests() {
  try {
    // Load the compiled WebAssembly module
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
    
    // Initialize the memory system
    exports.init();
    
    console.log('WebAssembly module loaded successfully');
    console.log('Memory management system initialized');
    
    // Test basic allocation
    console.log('\\nTesting basic allocation...');
    const ptr1 = exports.allocBlock(32);
    const ptr2 = exports.allocBlock(64);
    const ptr3 = exports.allocBlock(128);
    
    console.log(`Allocated 32 bytes at: ${ptr1}`);
    console.log(`Allocated 64 bytes at: ${ptr2}`);
    console.log(`Allocated 128 bytes at: ${ptr3}`);
    
    // Test rope node allocation
    console.log('\\nTesting rope node allocation...');
    const node1 = exports.allocRopeNode();
    const node2 = exports.allocRopeNode();
    const node3 = exports.allocRopeNode();
    
    console.log(`Allocated rope node at: ${node1}`);
    console.log(`Allocated rope node at: ${node2}`);
    console.log(`Allocated rope node at: ${node3}`);
    
    // Test freeing
    console.log('\\nTesting memory deallocation...');
    exports.freeBlock(ptr1, 32);
    exports.freeBlock(ptr2, 64);
    exports.freeBlock(ptr3, 128);
    
    exports.freeRopeNode(node1);
    exports.freeRopeNode(node2);
    exports.freeRopeNode(node3);
    
    console.log('Memory freed successfully');
    
    // Test reallocation
    console.log('\\nTesting reallocation...');
    const newPtr = exports.allocBlock(32);
    const newNode = exports.allocRopeNode();
    
    console.log(`Reallocated 32 bytes at: ${newPtr}`);
    console.log(`Reallocated rope node at: ${newNode}`);
    
    exports.freeBlock(newPtr, 32);
    exports.freeRopeNode(newNode);
    
    console.log('\\n✅ All memory management tests passed!');
    
  } catch (error) {
    console.error('❌ Test failed:', error);
    process.exit(1);
  }
}

runTests();
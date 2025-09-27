// Test rope operations in isolation to find the memory corruption

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function ropeOperationTests() {
  try {
    console.log('🌳 Rope Operation Tests');
    console.log('=======================');
    
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
    
    // Test 1: Single rope insertion
    console.log('\\n🧪 Test 1: Single Rope Insertion');
    try {
      console.log('  Testing insertText with "Hello"...');
      exports.insertText(0, "Hello");
      console.log('  ✅ First insertion succeeded');
      console.log(`  Length: ${exports.getLength()}`);
      
      // Test second insertion - this is where we expect failure
      console.log('\\n  Testing insertText with "World" at end...');
      exports.insertText(5, "World");
      console.log('  ✅ Second insertion succeeded');
      console.log(`  Length: ${exports.getLength()}`);
      
    } catch (e) {
      console.log(`  ❌ Rope insertion failed: ${e.message}`);
      console.log('  This confirms the bug is in rope operations, not allocator');
    }
    
    // Test 2: Reset and try hardcoded insert pattern
    console.log('\\n🧪 Test 2: Reset and Hardcoded Pattern');
    try {
      exports.resetSystem();
      console.log('  System reset');
      
      console.log('  First hardcoded insert...');
      let result = exports.applyHardcodedInsert(0);
      console.log(`  Result: ${result} (${result === 0 ? 'Success' : 'Failed'})`);
      console.log(`  Length: ${exports.getLength()}`);
      
      console.log('  Second hardcoded insert...');
      result = exports.applyHardcodedInsert(5);
      console.log(`  Result: ${result} (${result === 0 ? 'Success' : 'Failed'})`);
      console.log(`  Length: ${exports.getLength()}`);
      
    } catch (e) {
      console.log(`  ❌ Hardcoded pattern failed: ${e.message}`);
    }
    
    // Test 3: Single character insertions
    console.log('\\n🧪 Test 3: Single Character Insertions');
    try {
      exports.resetSystem();
      console.log('  System reset');
      
      const chars = ['H', 'e', 'l', 'l', 'o'];
      for (let i = 0; i < chars.length; i++) {
        console.log(`  Inserting '${chars[i]}' at position ${i}...`);
        exports.insertText(i, chars[i]);
        console.log(`    ✅ Success, length: ${exports.getLength()}`);
      }
      
    } catch (e) {
      console.log(`  ❌ Single character insertion failed: ${e.message}`);
      console.log(`  Failed at character index: likely where corruption occurs`);
    }
    
    // Test 4: Direct allocator calls during rope operations
    console.log('\\n🧪 Test 4: Manual Memory Tracking');
    try {
      exports.resetSystem();
      console.log('  System reset');
      
      console.log('  Pre-operation allocator test...');
      let testPtr = exports.allocBlock(64);
      console.log(`    Test allocation: ${testPtr}`);
      exports.freeBlock(testPtr, 64);
      console.log(`    Test free: succeeded`);
      
      console.log('  First rope operation...');
      exports.insertText(0, "A");
      console.log(`    Rope operation completed, length: ${exports.getLength()}`);
      
      console.log('  Post-operation allocator test...');
      testPtr = exports.allocBlock(64);
      console.log(`    Test allocation: ${testPtr}`);
      if (testPtr !== 0) {
        exports.freeBlock(testPtr, 64);
        console.log(`    Test free: succeeded`);
        console.log(`    ✅ Allocator state is healthy after first rope operation`);
      }
      
      console.log('  Second rope operation...');
      exports.insertText(1, "B");
      console.log(`    Rope operation completed, length: ${exports.getLength()}`);
      
      console.log('  Final allocator test...');
      testPtr = exports.allocBlock(64);
      console.log(`    Test allocation: ${testPtr}`);
      if (testPtr !== 0) {
        exports.freeBlock(testPtr, 64);
        console.log(`    Test free: succeeded`);
        console.log(`    ✅ Allocator state remains healthy`);
      } else {
        console.log(`    ❌ Allocator corrupted after second rope operation`);
      }
      
    } catch (e) {
      console.log(`  ❌ Memory tracking test failed: ${e.message}`);
    }
    
    console.log('\\n📊 Analysis');
    console.log('============');
    console.log('If allocator unit tests pass but rope operations fail,');
    console.log('the bug is in rope data structure management:');
    console.log('- Possible double-free in rope node deallocation');
    console.log('- Memory corruption in rope balancing operations');
    console.log('- Invalid pointer arithmetic in rope traversal');
    console.log('- Stack overflow in recursive rope operations');
    
  } catch (error) {
    console.error('❌ Rope operation tests failed:', error);
  }
}

ropeOperationTests();
// Comprehensive unit tests for BuddySystemAllocator
// Focus on isolating the memory corruption bug

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function allocatorTests() {
  try {
    console.log('🧪 BuddySystemAllocator Unit Tests');
    console.log('===================================');
    
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
    
    // Test 1: Basic allocation sequence
    console.log('\\n🧪 Test 1: Basic Allocation Sequence');
    console.log('Testing simple allocate-deallocate cycles...');
    
    const allocations = [];
    
    try {
      // Allocate several blocks of different sizes
      for (let i = 0; i < 5; i++) {
        const size = 32 + (i * 16); // 32, 48, 64, 80, 96 bytes
        console.log(`  Allocating ${size} bytes...`);
        const ptr = exports.allocBlock(size);
        console.log(`    Got pointer: ${ptr}`);
        
        if (ptr === 0) {
          console.log(`    ❌ Allocation ${i+1} failed`);
          break;
        } else {
          console.log(`    ✅ Allocation ${i+1} succeeded`);
          allocations.push({ ptr, size });
        }
      }
      
      console.log(`\\n  Successfully allocated ${allocations.length} blocks`);
      
      // Now free them in reverse order
      console.log('\\n  Freeing blocks in reverse order...');
      for (let i = allocations.length - 1; i >= 0; i--) {
        const { ptr, size } = allocations[i];
        console.log(`    Freeing ${size} bytes at ${ptr}...`);
        try {
          exports.freeBlock(ptr, size);
          console.log(`    ✅ Free ${i+1} succeeded`);
        } catch (e) {
          console.log(`    ❌ Free ${i+1} failed: ${e.message}`);
          break;
        }
      }
      
    } catch (e) {
      console.log(`❌ Test 1 failed: ${e.message}`);
    }
    
    // Test 2: Allocation stress test
    console.log('\\n🧪 Test 2: Allocation Stress Test');
    console.log('Testing many small allocations...');
    
    try {
      const smallAllocations = [];
      const blockSize = 32;
      let successCount = 0;
      
      for (let i = 0; i < 20; i++) {
        console.log(`  Small allocation ${i+1}/20...`);
        const ptr = exports.allocBlock(blockSize);
        
        if (ptr === 0) {
          console.log(`    ❌ Failed at allocation ${i+1}`);
          break;
        } else {
          console.log(`    ✅ Success: ${ptr}`);
          smallAllocations.push(ptr);
          successCount++;
        }
      }
      
      console.log(`\\n  Successfully allocated ${successCount} small blocks`);
      
      // Free all small blocks
      console.log('\\n  Freeing all small blocks...');
      for (let i = 0; i < smallAllocations.length; i++) {
        try {
          exports.freeBlock(smallAllocations[i], blockSize);
          console.log(`    ✅ Freed block ${i+1}`);
        } catch (e) {
          console.log(`    ❌ Failed to free block ${i+1}: ${e.message}`);
          break;
        }
      }
      
    } catch (e) {
      console.log(`❌ Test 2 failed: ${e.message}`);
    }
    
    // Test 3: Mixed size allocation pattern
    console.log('\\n🧪 Test 3: Mixed Size Allocation Pattern');
    console.log('Testing allocation pattern similar to rope operations...');
    
    try {
      const mixedAllocations = [];
      
      // Simulate rope node allocation (64 bytes) + text allocation (variable)
      for (let i = 0; i < 3; i++) {
        console.log(`\\n  Operation ${i+1}:`);
        
        // Allocate rope node
        console.log(`    Allocating rope node (64 bytes)...`);
        const nodePtr = exports.allocBlock(64);
        if (nodePtr === 0) {
          console.log(`    ❌ Rope node allocation failed`);
          break;
        }
        console.log(`    ✅ Rope node: ${nodePtr}`);
        
        // Allocate text buffer (variable size)
        const textSize = 20 + (i * 10); // 20, 30, 40 bytes
        console.log(`    Allocating text buffer (${textSize} bytes)...`);
        const textPtr = exports.allocBlock(textSize);
        if (textPtr === 0) {
          console.log(`    ❌ Text buffer allocation failed`);
          // Free the node we just allocated
          exports.freeBlock(nodePtr, 64);
          break;
        }
        console.log(`    ✅ Text buffer: ${textPtr}`);
        
        mixedAllocations.push({ nodePtr, textPtr, textSize });
      }
      
      console.log(`\\n  Successfully completed ${mixedAllocations.length} mixed operations`);
      
      // Free in realistic order (text first, then nodes)
      console.log('\\n  Freeing in realistic order...');
      for (let i = 0; i < mixedAllocations.length; i++) {
        const { nodePtr, textPtr, textSize } = mixedAllocations[i];
        console.log(`    Freeing operation ${i+1}...`);
        
        try {
          exports.freeBlock(textPtr, textSize);
          console.log(`      ✅ Text buffer freed`);
          exports.freeBlock(nodePtr, 64);
          console.log(`      ✅ Rope node freed`);
        } catch (e) {
          console.log(`      ❌ Failed to free operation ${i+1}: ${e.message}`);
          break;
        }
      }
      
    } catch (e) {
      console.log(`❌ Test 3 failed: ${e.message}`);
    }
    
    // Test 4: Exact failure reproduction
    console.log('\\n🧪 Test 4: Exact Failure Reproduction');
    console.log('Reproducing the exact sequence that causes the bug...');
    
    try {
      console.log('  Step 1: Simulate first transaction...');
      
      // First transaction allocations (string + rope operations)
      const stringPtr1 = exports.allocBlock(70); // JSON string
      console.log(`    JSON string allocation: ${stringPtr1}`);
      
      if (stringPtr1 !== 0) {
        const ropePtr1 = exports.allocBlock(64); // Rope node
        console.log(`    Rope node allocation: ${ropePtr1}`);
        
        if (ropePtr1 !== 0) {
          const textPtr1 = exports.allocBlock(20); // Text content
          console.log(`    Text content allocation: ${textPtr1}`);
          
          // Free as transaction completes
          exports.freeBlock(stringPtr1, 70);
          console.log(`    ✅ Freed JSON string`);
          
          // Keep rope structures (they persist)
          console.log(`    Rope structures remain allocated`);
          
          console.log('\\n  Step 2: Simulate second transaction...');
          
          // Second transaction - this is where it fails
          const stringPtr2 = exports.allocBlock(68); // Second JSON string
          console.log(`    Second JSON string allocation: ${stringPtr2}`);
          
          if (stringPtr2 === 0) {
            console.log(`    ❌ REPRODUCED: Second allocation failed!`);
            console.log(`    This confirms the allocator state is corrupted.`);
          } else {
            console.log(`    ✅ Second allocation succeeded: ${stringPtr2}`);
            console.log(`    Bug not reproduced in this test.`);
          }
        }
      }
      
    } catch (e) {
      console.log(`❌ Test 4 failed: ${e.message}`);
      console.log(`This confirms the allocator corruption bug.`);
    }
    
    console.log('\\n📊 Test Summary');
    console.log('================');
    console.log('These tests will help isolate exactly where the allocator fails.');
    console.log('Next step: Add debug output to the allocator implementation.');
    
  } catch (error) {
    console.error('❌ Allocator tests failed:', error);
  }
}

allocatorTests();
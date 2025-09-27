// Comprehensive test for string marshalling solution

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// WASM string utilities (same as ClojureScript version)
function readWasmString(wasmInstance, ptr, maxLength = 10000) {
  if (ptr === 0) return "";
  
  const memory = wasmInstance.exports.memory;
  const buffer = new Uint16Array(memory.buffer);
  const uint16Offset = ptr / 2;
  
  let str = "";
  let i = 0;
  
  while (i < maxLength && i < (buffer.length - uint16Offset)) {
    const charCode = buffer[uint16Offset + i];
    if (charCode === 0) break; // Null terminator
    str += String.fromCharCode(charCode);
    i++;
  }
  
  return str;
}

function readWasmStringWithLength(wasmInstance, ptr, length) {
  if (ptr === 0 || length === 0) return "";
  
  const memory = wasmInstance.exports.memory;
  const buffer = new Uint16Array(memory.buffer);
  const uint16Offset = ptr / 2;
  
  let str = "";
  for (let i = 0; i < length && i < (buffer.length - uint16Offset); i++) {
    const charCode = buffer[uint16Offset + i];
    str += String.fromCharCode(charCode);
  }
  
  return str;
}

// Safe text range getter (mirroring ClojureScript implementation)
function getTextRangeSafe(wasmInstance, start, end) {
  try {
    const exports = wasmInstance.exports;
    const length = exports.getLength();
    const clampedStart = Math.max(0, Math.min(start, length));
    const clampedEnd = Math.max(clampedStart, Math.min(end, length));
    const rangeLength = clampedEnd - clampedStart;
    
    if (rangeLength === 0) {
      return ["", true];
    }
    
    const ptr = exports.getTextInRange(clampedStart, clampedEnd);
    const text = (typeof ptr === 'number') 
      ? readWasmStringWithLength(wasmInstance, ptr, rangeLength)
      : String(ptr); // Fallback if string returned directly
    
    return [text, true];
  } catch (error) {
    console.error('Failed to get text range:', error.message);
    return ["", false];
  }
}

async function stringMarshallingTest() {
  try {
    console.log('🧪 Comprehensive String Marshalling Test');
    console.log('==========================================');
    
    const wasmPath = path.join(__dirname, '../build/debug.wasm');
    const wasmBuffer = fs.readFileSync(wasmPath);
    const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
      env: {
        abort: () => {
          throw new Error('WebAssembly aborted');
        }
      }
    });
    
    const wasmInstance = wasmModule.instance;
    const { exports } = wasmInstance;
    
    // Initialize
    exports.init();
    console.log('✅ WASM initialized');
    
    // Test 1: Insert multiple pieces of text
    console.log('\\n📝 Test 1: Building Complex Text');
    let errorCode = exports.applyHardcodedInsert(0); // Inserts "Hello"
    console.log('Insert "Hello":', errorCode === 0 ? '✅' : '❌');
    
    // Use JSON transaction to add more text
    const addSpace = '{"type":0,"position":5,"text":" "}';
    errorCode = exports.applyTransaction(addSpace);
    console.log('Add space:', errorCode === 0 ? '✅' : '❌');
    
    const addWorld = '{"type":0,"position":6,"text":"World!"}';
    errorCode = exports.applyTransaction(addWorld);
    console.log('Add "World!":', errorCode === 0 ? '✅' : '❌');
    
    const totalLength = exports.getLength();
    console.log(`Total length: ${totalLength}`);
    
    // Test 2: Safe string reading at various ranges
    console.log('\\n🔍 Test 2: Range-based Text Retrieval');
    
    const testRanges = [
      [0, 5],     // "Hello"
      [5, 6],     // " "
      [6, 12],    // "World!"
      [0, 12],    // Full text
      [3, 8],     // "lo Wo"
      [10, 12],   // "d!"
      [0, 100],   // Beyond range (should clamp)
    ];
    
    for (const [start, end] of testRanges) {
      const [text, success] = getTextRangeSafe(wasmInstance, start, end);
      if (success) {
        console.log(`Range [${start}-${end}]: "${text}" ✅`);
      } else {
        console.log(`Range [${start}-${end}]: FAILED ❌`);
      }
    }
    
    // Test 3: Character-by-character verification
    console.log('\\n🔤 Test 3: Character-by-Character Verification');
    for (let i = 0; i < Math.min(totalLength, 15); i++) {
      const charCode = exports.getCharacterAt(i);
      const char = String.fromCharCode(charCode);
      console.log(`Position ${i}: '${char}' (${charCode})`);
    }
    
    // Test 4: Virtual scrolling simulation
    console.log('\\n📜 Test 4: Virtual Scrolling Simulation');
    const chunkSize = 4;
    const chunks = [];
    
    for (let start = 0; start < totalLength; start += chunkSize) {
      const end = Math.min(start + chunkSize, totalLength);
      const [chunk, success] = getTextRangeSafe(wasmInstance, start, end);
      if (success) {
        chunks.push(`[${start}-${end}]: "${chunk}"`);
      }
    }
    
    console.log('Virtual scroll chunks:');
    chunks.forEach(chunk => console.log(`  ${chunk}`));
    
    // Test 5: Performance test
    console.log('\\n⚡ Test 5: Performance Test');
    const iterations = 1000;
    const testStart = performance.now();
    
    for (let i = 0; i < iterations; i++) {
      const start = Math.floor(Math.random() * totalLength);
      const end = Math.min(start + Math.floor(Math.random() * 5) + 1, totalLength);
      getTextRangeSafe(wasmInstance, start, end);
    }
    
    const testEnd = performance.now();
    const avgTime = (testEnd - testStart) / iterations;
    console.log(`${iterations} random range reads: ${avgTime.toFixed(3)}ms average`);
    
    // Test 6: Delete and modify operations
    console.log('\\n✏️ Test 6: Text Modification');
    
    // Delete "World" and replace with "Universe"
    const deleteWorld = '{"type":1,"position":6,"length":5}'; // Delete "World"
    errorCode = exports.applyTransaction(deleteWorld);
    console.log('Delete "World":', errorCode === 0 ? '✅' : '❌');
    
    const [textAfterDelete] = getTextRangeSafe(wasmInstance, 0, exports.getLength());
    console.log(`After delete: "${textAfterDelete}"`);
    
    const insertUniverse = '{"type":0,"position":6,"text":"Universe"}';
    errorCode = exports.applyTransaction(insertUniverse);
    console.log('Insert "Universe":', errorCode === 0 ? '✅' : '❌');
    
    const [finalText] = getTextRangeSafe(wasmInstance, 0, exports.getLength());
    console.log(`Final text: "${finalText}"`);
    
    console.log('\\n🎉 String Marshalling Test Complete!');
    console.log('All L1.2 text operations ready for implementation.');
    
  } catch (error) {
    console.error('❌ Test failed:', error);
    process.exit(1);
  }
}

stringMarshallingTest();
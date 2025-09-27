// Debug JSON parsing issue step by step

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function debugJson() {
  try {
    console.log('🔍 JSON Parsing Debug');
    
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
    
    // Test string passing directly
    console.log('\n🧪 Testing string parameter passing...');
    
    // Test with hardcoded insert first (this works)
    console.log('1. Hardcoded insert test:');
    const hardcodedResult = exports.applyHardcodedInsert(0);
    console.log('   Result:', hardcodedResult, '(0 = success)');
    console.log('   Length:', exports.getLength());
    
    // Test string passing to simpleInsert
    console.log('\n2. Simple string parameter test:');
    try {
      const simpleResult = exports.applySimpleInsert(5, 'X');
      console.log('   Simple insert result:', simpleResult);
      console.log('   New length:', exports.getLength());
    } catch (e) {
      console.log('   Simple insert failed:', e.message);
    }
    
    // Test JSON string passing - start simple
    console.log('\n3. JSON string parameter test:');
    
    // Try the simplest possible JSON
    const jsonTests = [
      '{}',
      '{"a":1}',
      '{"type":0}',
      '{"type":0,"position":0}',
      '{"type":0,"position":0,"text":"A"}',
    ];
    
    for (const json of jsonTests) {
      try {
        console.log(`   Testing JSON: ${json}`);
        const result = exports.applyTransaction(json);
        console.log(`   Result: ${result} ✅`);
      } catch (e) {
        console.log(`   Failed: ${e.message} ❌`);
        break; // Stop at first failure
      }
    }
    
  } catch (error) {
    console.error('❌ Debug failed:', error);
  }
}

debugJson();
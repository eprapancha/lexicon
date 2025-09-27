// Comprehensive test suite for Rope data structure

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function runRopeTests() {
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
    console.log('✅ WebAssembly module and Rope initialized');
    
    // Test 1: Basic insertion and retrieval
    console.log('\\n🧪 Test 1: Basic insertion and retrieval');
    exports.insertText(0, 'Hello');
    console.log(`Text after insert: "${exports.getText()}"`);
    console.log(`Length: ${exports.getLength()}`);
    
    exports.insertText(5, ' World');
    console.log(`Text after second insert: "${exports.getText()}"`);
    console.log(`Length: ${exports.getLength()}`);
    
    // Test 2: Insertion in the middle
    console.log('\\n🧪 Test 2: Insertion in the middle');
    exports.insertText(5, ', Beautiful');
    console.log(`Text after middle insert: "${exports.getText()}"`);
    console.log(`Length: ${exports.getLength()}`);
    
    // Test 3: Deletion
    console.log('\\n🧪 Test 3: Deletion');
    const beforeDelete = exports.getText();
    console.log(`Before deletion: "${beforeDelete}"`);
    
    exports.delete(5, 16); // Delete ", Beautiful"
    console.log(`After deletion: "${exports.getText()}"`);
    console.log(`Length: ${exports.getLength()}`);
    
    // Test 4: Large text insertion (test chunking)
    console.log('\\n🧪 Test 4: Large text insertion');
    const largeText = 'A'.repeat(1000); // 1000 characters
    exports.insertText(0, largeText);
    
    const resultLength = exports.getLength();
    console.log(`Length after large insert: ${resultLength}`);
    console.log(`First 50 chars: "${exports.getText().substring(0, 50)}"`);
    console.log(`Last 50 chars: "${exports.getText().substring(resultLength - 50)}"`);
    
    // Test 5: Multiple small insertions
    console.log('\\n🧪 Test 5: Multiple small insertions');
    
    // Reset rope
    exports.init();
    
    const words = ['The', ' quick', ' brown', ' fox', ' jumps', ' over', ' the', ' lazy', ' dog'];
    let position = 0;
    
    for (const word of words) {
      exports.insertText(position, word);
      position += word.length;
      console.log(`After inserting "${word}": "${exports.getText()}"`);
    }
    
    // Test 6: Random insertions and deletions
    console.log('\\n🧪 Test 6: Random operations stress test');
    
    exports.init(); // Reset
    
    // Build up some initial text
    exports.insertText(0, 'Initial text for stress testing');
    
    let operations = 0;
    const maxOperations = 20;
    
    for (let i = 0; i < maxOperations; i++) {
      const currentLength = exports.getLength();
      const operation = Math.random() < 0.7 ? 'insert' : 'delete'; // 70% insert, 30% delete
      
      if (operation === 'insert') {
        const position = Math.floor(Math.random() * (currentLength + 1));
        const text = `[${i}]`;
        exports.insertText(position, text);
        operations++;
        console.log(`Op ${operations}: Insert "${text}" at pos ${position}, len=${exports.getLength()}`);
      } else if (currentLength > 0) {
        const start = Math.floor(Math.random() * currentLength);
        const maxLength = Math.min(5, currentLength - start);
        const length = Math.floor(Math.random() * maxLength) + 1;
        exports.delete(start, start + length);
        operations++;
        console.log(`Op ${operations}: Delete from ${start} to ${start + length}, len=${exports.getLength()}`);
      }
      
      // Verify integrity occasionally
      if (i % 5 === 0) {
        const text = exports.getText();
        const length = exports.getLength();
        console.log(`  Integrity check: text.length=${text.length}, reported=${length}`);
        
        if (text.length !== length) {
          throw new Error(`Length mismatch: text.length=${text.length}, reported=${length}`);
        }
      }
    }
    
    console.log(`\\nFinal text: "${exports.getText()}"`);
    console.log(`Final length: ${exports.getLength()}`);
    
    // Test 7: UTF-16 character handling
    console.log('\\n🧪 Test 7: UTF-16 character handling');
    
    exports.init(); // Reset
    
    const unicodeText = 'Hello 🌍 World! 🚀 Lexicon';
    exports.insertText(0, unicodeText);
    
    console.log(`Unicode text: "${exports.getText()}"`);
    console.log(`Length: ${exports.getLength()}`);
    console.log(`JavaScript length: ${unicodeText.length}`);
    
    // Test 8: Edge cases
    console.log('\\n🧪 Test 8: Edge cases');
    
    exports.init(); // Reset
    
    // Insert empty string
    exports.insertText(0, '');
    console.log(`After empty insert: "${exports.getText()}" (length: ${exports.getLength()})`);
    
    // Insert at position 0 when empty
    exports.insertText(0, 'First');
    console.log(`After first insert: "${exports.getText()}"`);
    
    // Insert at end
    exports.insertText(exports.getLength(), ' Last');
    console.log(`After end insert: "${exports.getText()}"`);
    
    // Delete beyond bounds (should be safe)
    exports.delete(100, 200);
    console.log(`After out-of-bounds delete: "${exports.getText()}"`);
    
    // Delete with zero length
    exports.delete(2, 2);
    console.log(`After zero-length delete: "${exports.getText()}"`);
    
    console.log('\\n✅ All Rope tests completed successfully!');
    
  } catch (error) {
    console.error('❌ Rope test failed:', error);
    process.exit(1);
  }
}

runRopeTests();
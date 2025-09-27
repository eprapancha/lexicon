// Comprehensive test suite for new transaction system and error handling

import fs from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function runTransactionTests() {
  try {
    console.log('🧪 Testing New Transaction System & Error Handling');
    console.log('=================================================');
    
    const wasmPath = path.join(__dirname, '../build/debug.wasm');
    
    if (!fs.existsSync(wasmPath)) {
      console.error('WASM module not found. Please run: npm run asbuild:debug');
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
    console.log('✅ WASM module loaded and initialized');
    
    // Test 1: Basic Transaction System
    console.log('\\n🧪 Test 1: Basic Transaction Operations');
    
    // Insert transaction
    let transaction = JSON.stringify({type: 0, position: 0, text: "Hello World"});
    let errorCode = exports.applyTransaction(transaction);
    console.log(`Insert transaction result: ${errorCode} (0 = success)`);
    
    if (errorCode === 0) {
      let result = JSON.parse(exports.getLastResult());
      console.log(`Result: cursor=${result.cursorPosition}, length=${result.length}`);
      console.log(`Current text: "${exports.getText()}"`);
    } else {
      console.log(`Error: ${exports.getLastErrorMessage()}`);
    }
    
    // Test 2: Error Handling
    console.log('\\n🧪 Test 2: Error Handling');
    
    // Invalid position
    transaction = JSON.stringify({type: 0, position: 1000, text: "Invalid"});
    errorCode = exports.applyTransaction(transaction);
    console.log(`Invalid position error code: ${errorCode}`);
    console.log(`Error message: "${exports.getLastErrorMessage()}"`);
    
    // Invalid transaction type
    transaction = JSON.stringify({type: 99, position: 0, text: "Invalid"});
    errorCode = exports.applyTransaction(transaction);
    console.log(`Invalid transaction type error code: ${errorCode}`);
    console.log(`Error message: "${exports.getLastErrorMessage()}"`);
    
    // Test 3: Delete Operations
    console.log('\\n🧪 Test 3: Delete Operations');
    
    transaction = JSON.stringify({type: 1, position: 6, length: 5}); // Delete "World"
    errorCode = exports.applyTransaction(transaction);
    console.log(`Delete transaction result: ${errorCode}`);
    
    if (errorCode === 0) {
      let result = JSON.parse(exports.getLastResult());
      console.log(`After delete: cursor=${result.cursorPosition}, length=${result.length}`);
      console.log(`Current text: "${exports.getText()}"`);
    }
    
    // Test 4: Replace Operations  
    console.log('\\n🧪 Test 4: Replace Operations');
    
    transaction = JSON.stringify({type: 2, position: 0, length: 5, text: "Hi"}); // Replace "Hello" with "Hi"
    errorCode = exports.applyTransaction(transaction);
    console.log(`Replace transaction result: ${errorCode}`);
    
    if (errorCode === 0) {
      let result = JSON.parse(exports.getLastResult());
      console.log(`After replace: cursor=${result.cursorPosition}, length=${result.length}`);
      console.log(`Current text: "${exports.getText()}"`);
    }
    
    // Test 5: Range-based Text Access
    console.log('\\n🧪 Test 5: Range-based Text Access');
    
    // Add more content for range testing
    transaction = JSON.stringify({type: 0, position: 3, text: " there! How are you doing today? This is a longer text for testing range access."});
    exports.applyTransaction(transaction);
    
    const fullText = exports.getText();
    console.log(`Full text length: ${fullText.length}`);
    console.log(`Full text: "${fullText}"`);
    
    // Test getTextInRange
    const range1 = exports.getTextInRange(0, 10);
    const range2 = exports.getTextInRange(10, 20);
    const range3 = exports.getTextInRange(20, -1); // Should handle gracefully
    
    console.log(`Range [0-10]: "${range1}"`);
    console.log(`Range [10-20]: "${range2}"`);
    console.log(`Range [20-end]: "${range3}"`);
    
    // Test 6: Character Access
    console.log('\\n🧪 Test 6: Character Access');
    
    for (let i = 0; i < Math.min(10, fullText.length); i++) {
      const char = exports.getCharacterAt(i);
      const jsChar = fullText.charCodeAt(i);
      console.log(`Char at ${i}: WASM=${char}, JS=${jsChar}, char='${String.fromCharCode(char)}'`);
      
      if (char !== jsChar) {
        console.log(`❌ Character mismatch at position ${i}`);
      }
    }
    
    // Test 7: Document Statistics
    console.log('\\n🧪 Test 7: Document Statistics');
    
    const stats = JSON.parse(exports.getDocumentStats());
    console.log(`Document stats:`, stats);
    
    // Test 8: Edge Cases
    console.log('\\n🧪 Test 8: Edge Cases');
    
    // Empty text insert
    transaction = JSON.stringify({type: 0, position: 0, text: ""});
    errorCode = exports.applyTransaction(transaction);
    console.log(`Empty insert result: ${errorCode}`);
    
    // Zero-length delete
    transaction = JSON.stringify({type: 1, position: 0, length: 0});
    errorCode = exports.applyTransaction(transaction);
    console.log(`Zero-length delete result: ${errorCode}`);
    console.log(`Error message: "${exports.getLastErrorMessage()}"`);
    
    // Out of bounds delete
    transaction = JSON.stringify({type: 1, position: 0, length: 10000});
    errorCode = exports.applyTransaction(transaction);
    console.log(`Out of bounds delete result: ${errorCode}`);
    console.log(`Error message: "${exports.getLastErrorMessage()}"`);
    
    // Test 9: Memory Stress Test
    console.log('\\n🧪 Test 9: Memory Stress Test');
    
    let successfulOps = 0;
    const stressTestOps = 100;
    
    for (let i = 0; i < stressTestOps; i++) {
      const opType = Math.random() < 0.7 ? 0 : 1; // 70% insert, 30% delete
      const currentLength = exports.getLength();
      
      if (opType === 0) { // Insert
        const position = Math.floor(Math.random() * (currentLength + 1));
        const text = `[${i}]`;
        transaction = JSON.stringify({type: 0, position, text});
      } else { // Delete
        if (currentLength > 0) {
          const position = Math.floor(Math.random() * currentLength);
          const maxLength = Math.min(5, currentLength - position);
          const length = Math.floor(Math.random() * maxLength) + 1;
          transaction = JSON.stringify({type: 1, position, length});
        } else {
          continue; // Skip if nothing to delete
        }
      }
      
      errorCode = exports.applyTransaction(transaction);
      if (errorCode === 0) {
        successfulOps++;
      } else if (i < 5) { // Log first few errors
        console.log(`Operation ${i} failed: ${exports.getLastErrorMessage()}`);
      }
    }
    
    console.log(`Stress test: ${successfulOps}/${stressTestOps} operations successful`);
    console.log(`Final text length: ${exports.getLength()}`);
    
    // Test 10: Transaction Atomicity
    console.log('\\n🧪 Test 10: Transaction Atomicity');
    
    const beforeLength = exports.getLength();
    const beforeText = exports.getText();
    
    // Try an invalid transaction that should fail completely
    transaction = JSON.stringify({type: 1, position: -1, length: 5}); // Invalid position
    errorCode = exports.applyTransaction(transaction);
    
    const afterLength = exports.getLength();
    const afterText = exports.getText();
    
    console.log(`Before failed transaction: length=${beforeLength}`);
    console.log(`After failed transaction: length=${afterLength}`);
    console.log(`Text unchanged: ${beforeText === afterText ? '✅' : '❌'}`);
    console.log(`Length unchanged: ${beforeLength === afterLength ? '✅' : '❌'}`);
    
    console.log('\\n🎉 All transaction system tests completed!');
    
  } catch (error) {
    console.error('❌ Transaction test failed:', error);
    process.exit(1);
  }
}

runTransactionTests();
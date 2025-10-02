// Tree-sitter Parser Web Worker
// Handles syntax parsing in a separate thread to avoid blocking the UI

let TreeSitter = null;
let parser = null;
let tree = null;
let language = null;

// Import web-tree-sitter
importScripts('https://tree-sitter.github.io/tree-sitter/assets/js/tree-sitter.js');

// Initialize the worker
self.onmessage = async function(e) {
  const { type, payload } = e.data;
  
  try {
    switch (type) {
      case 'init':
        await initializeParser(payload);
        break;
      case 'parse':
        await parseText(payload);
        break;
      case 'edit':
        await editAndReparse(payload);
        break;
      default:
        console.warn('Unknown message type:', type);
    }
  } catch (error) {
    console.error('Parser worker error:', error);
    self.postMessage({
      type: 'error',
      payload: {
        error: error.message,
        stack: error.stack
      }
    });
  }
};

async function initializeParser(payload) {
  const { languageName, wasmPath } = payload;
  
  console.log('Initializing Tree-sitter parser for', languageName);
  
  try {
    // Initialize Tree-sitter
    await TreeSitter.init();
    TreeSitter = TreeSitter.default || TreeSitter;
    
    // Create parser
    parser = new TreeSitter();
    
    // Load language grammar
    if (wasmPath) {
      // For development, we'll simulate loading a language
      // In production, this would load the actual WASM file:
      // language = await TreeSitter.Language.load(wasmPath);
      console.log('Simulating language load for', languageName);
      language = { name: languageName }; // Mock language object
    }
    
    // Set language on parser (in production)
    // parser.setLanguage(language);
    
    self.postMessage({
      type: 'init-success',
      payload: {
        languageName,
        ready: true
      }
    });
    
  } catch (error) {
    console.error('Failed to initialize parser:', error);
    self.postMessage({
      type: 'init-error',
      payload: {
        error: error.message
      }
    });
  }
}

async function parseText(payload) {
  const { text, languageName } = payload;
  
  if (!parser) {
    throw new Error('Parser not initialized');
  }
  
  console.log('Parsing text of length:', text.length);
  
  try {
    // For development, we'll create a mock AST
    // In production, this would be: tree = parser.parse(text);
    tree = createMockAST(text, languageName);
    
    // Serialize the tree for sending back to main thread
    const serializedTree = serializeTree(tree);
    
    self.postMessage({
      type: 'parse-success',
      payload: {
        ast: serializedTree,
        languageName
      }
    });
    
  } catch (error) {
    console.error('Parse error:', error);
    self.postMessage({
      type: 'parse-error',
      payload: {
        error: error.message
      }
    });
  }
}

async function editAndReparse(payload) {
  const { edit, text } = payload;
  
  if (!parser || !tree) {
    // Fall back to full parse
    await parseText({ text, languageName: language?.name || 'unknown' });
    return;
  }
  
  console.log('Incremental reparse with edit:', edit);
  
  try {
    // For development, simulate incremental parsing
    // In production, this would be:
    // tree.edit(edit);
    // tree = parser.parse(text, tree);
    
    tree = createMockAST(text, language?.name || 'unknown');
    
    const serializedTree = serializeTree(tree);
    
    self.postMessage({
      type: 'edit-success',
      payload: {
        ast: serializedTree
      }
    });
    
  } catch (error) {
    console.error('Incremental parse error:', error);
    self.postMessage({
      type: 'edit-error',
      payload: {
        error: error.message
      }
    });
  }
}

// Mock AST creation for development
function createMockAST(text, languageName) {
  const lines = text.split('\n');
  const nodes = [];
  
  // Simple keyword detection for JavaScript
  if (languageName === 'javascript') {
    const keywords = ['const', 'let', 'var', 'function', 'class', 'if', 'else', 'for', 'while', 'return'];
    const strings = [];
    const comments = [];
    
    lines.forEach((line, lineIndex) => {
      let columnIndex = 0;
      
      // Find keywords
      keywords.forEach(keyword => {
        let index = 0;
        while ((index = line.indexOf(keyword, index)) !== -1) {
          if (isWordBoundary(line, index, keyword.length)) {
            nodes.push({
              type: 'keyword',
              text: keyword,
              startPosition: { row: lineIndex, column: index },
              endPosition: { row: lineIndex, column: index + keyword.length }
            });
          }
          index += keyword.length;
        }
      });
      
      // Find strings
      let inString = false;
      let stringStart = 0;
      let quote = '';
      
      for (let i = 0; i < line.length; i++) {
        if (!inString && (line[i] === '"' || line[i] === "'" || line[i] === '`')) {
          inString = true;
          stringStart = i;
          quote = line[i];
        } else if (inString && line[i] === quote && line[i-1] !== '\\') {
          nodes.push({
            type: 'string',
            text: line.substring(stringStart, i + 1),
            startPosition: { row: lineIndex, column: stringStart },
            endPosition: { row: lineIndex, column: i + 1 }
          });
          inString = false;
        }
      }
      
      // Find comments
      const commentIndex = line.indexOf('//');
      if (commentIndex !== -1) {
        nodes.push({
          type: 'comment',
          text: line.substring(commentIndex),
          startPosition: { row: lineIndex, column: commentIndex },
          endPosition: { row: lineIndex, column: line.length }
        });
      }
    });
  }
  
  return {
    type: 'source_file',
    children: nodes,
    text: text
  };
}

function isWordBoundary(text, start, length) {
  const before = start > 0 ? text[start - 1] : ' ';
  const after = start + length < text.length ? text[start + length] : ' ';
  return /\s/.test(before) && /\s/.test(after);
}

function serializeTree(tree) {
  // Convert tree to JSON-serializable format
  return {
    type: tree.type,
    children: tree.children || [],
    text: tree.text
  };
}

console.log('Parser worker initialized');
// Tree-sitter Parser Web Worker
// Handles syntax parsing in a separate thread to avoid blocking the UI

let TreeSitter = null;
let parser = null;
let tree = null;
let language = null;
let mockMode = false; // Flag to indicate we're running in mock mode

// Try to import web-tree-sitter, fall back to mock mode if it fails
try {
  importScripts('https://tree-sitter.github.io/tree-sitter/assets/js/tree-sitter.js');
  console.log('Tree-sitter library loaded successfully');
} catch (error) {
  console.warn('Failed to load Tree-sitter library, falling back to mock mode:', error);
  mockMode = true;
}

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
  
  console.log('Initializing Tree-sitter parser for', languageName, mockMode ? '(mock mode)' : '(real mode)');
  
  try {
    if (!mockMode && TreeSitter) {
      // Real Tree-sitter mode
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
    } else {
      // Mock mode - simulate initialization
      console.log('Mock mode: Simulating parser initialization for', languageName);
      language = { name: languageName, mock: true };
    }
    
    self.postMessage({
      type: 'init-success',
      payload: {
        languageName,
        ready: true,
        mockMode: mockMode
      }
    });
    
  } catch (error) {
    console.error('Failed to initialize parser:', error);
    self.postMessage({
      type: 'init-error',
      payload: {
        error: error.message,
        mockMode: mockMode
      }
    });
  }
}

async function parseText(payload) {
  const { text, languageName } = payload;
  
  if (!language) {
    throw new Error('Parser not initialized');
  }
  
  console.log('Parsing text of length:', text.length, mockMode ? '(mock mode)' : '(real mode)');
  
  try {
    if (!mockMode && parser) {
      // Real Tree-sitter parsing (when available)
      // tree = parser.parse(text);
      // For now, still using mock until we have real WASM files
      tree = createMockAST(text, languageName);
    } else {
      // Mock mode parsing
      tree = createMockAST(text, languageName);
    }
    
    // Serialize the tree for sending back to main thread
    const serializedTree = serializeTree(tree);
    
    self.postMessage({
      type: 'parse-success',
      payload: {
        ast: serializedTree,
        languageName,
        mockMode: mockMode
      }
    });
    
  } catch (error) {
    console.error('Parse error:', error);
    self.postMessage({
      type: 'parse-error',
      payload: {
        error: error.message,
        mockMode: mockMode
      }
    });
  }
}

async function editAndReparse(payload) {
  const { edit, text } = payload;
  
  if ((!mockMode && (!parser || !tree)) || (mockMode && !language)) {
    // Fall back to full parse
    await parseText({ text, languageName: language?.name || 'unknown' });
    return;
  }
  
  console.log('Incremental reparse with edit:', edit, mockMode ? '(mock mode)' : '(real mode)');
  
  try {
    if (!mockMode && parser && tree) {
      // Real incremental parsing (when available)
      // tree.edit(edit);
      // tree = parser.parse(text, tree);
      // For now, still using mock until we have real WASM files
      tree = createMockAST(text, language?.name || 'unknown');
    } else {
      // Mock mode incremental parsing
      tree = createMockAST(text, language?.name || 'unknown');
    }
    
    const serializedTree = serializeTree(tree);
    
    self.postMessage({
      type: 'edit-success',
      payload: {
        ast: serializedTree,
        mockMode: mockMode
      }
    });
    
  } catch (error) {
    console.error('Incremental parse error:', error);
    self.postMessage({
      type: 'edit-error',
      payload: {
        error: error.message,
        mockMode: mockMode
      }
    });
  }
}

// Mock AST creation for development
function createMockAST(text, languageName) {
  const lines = text.split('\n');
  const nodes = [];
  
  // Enhanced JavaScript syntax detection
  if (languageName === 'javascript') {
    const keywords = [
      'const', 'let', 'var', 'function', 'class', 'if', 'else', 'for', 'while', 'return',
      'import', 'export', 'from', 'default', 'async', 'await', 'try', 'catch', 'finally',
      'throw', 'new', 'this', 'super', 'extends', 'static', 'get', 'set', 'typeof',
      'instanceof', 'in', 'of', 'do', 'switch', 'case', 'break', 'continue', 'null',
      'undefined', 'true', 'false', 'with', 'debugger', 'delete', 'void'
    ];
    
    lines.forEach((line, lineIndex) => {
      // Skip if line is empty
      if (line.trim().length === 0) return;
      
      // Find keywords (improved word boundary detection)
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
      
      // Find strings (improved handling)
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
      
      // Find numbers
      const numberRegex = /\b\d+(\.\d+)?\b/g;
      let numberMatch;
      while ((numberMatch = numberRegex.exec(line)) !== null) {
        nodes.push({
          type: 'number',
          text: numberMatch[0],
          startPosition: { row: lineIndex, column: numberMatch.index },
          endPosition: { row: lineIndex, column: numberMatch.index + numberMatch[0].length }
        });
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
  // Check that the character before is not alphanumeric and after is not alphanumeric
  return !/[a-zA-Z0-9_$]/.test(before) && !/[a-zA-Z0-9_$]/.test(after);
}

function serializeTree(tree) {
  // Convert tree to JSON-serializable format
  return {
    type: tree.type,
    children: tree.children || [],
    text: tree.text
  };
}

console.log('Parser worker initialized in', mockMode ? 'mock mode' : 'real mode');
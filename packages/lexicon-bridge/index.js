#!/usr/bin/env node

const WebSocket = require('ws');
const { spawn } = require('child_process');
const path = require('path');

class LSPBridge {
  constructor(port = 30303) {
    this.port = port;
    this.lspProcesses = new Map(); // language -> { process, clients }
    this.clients = new Set();
    this.setupServer();
  }

  setupServer() {
    this.wss = new WebSocket.Server({ 
      port: this.port,
      host: 'localhost'
    });

    this.wss.on('connection', (ws) => {
      console.log('✅ Client connected');
      this.clients.add(ws);
      
      // Test: Send a ping message to confirm connection works
      ws.send(JSON.stringify({
        type: 'ping',
        message: 'Bridge server ready'
      }));

      ws.on('message', async (data) => {
        try {
          console.log('📨 Bridge: Received raw message:', data.toString());
          const message = JSON.parse(data.toString());
          console.log('📨 Bridge: Parsed message:', JSON.stringify(message, null, 2));
          await this.handleMessage(ws, message);
        } catch (error) {
          console.error('❌ Bridge: Error handling message:', error);
          this.sendError(ws, 'Invalid JSON message');
        }
      });

      ws.on('close', () => {
        console.log('Client disconnected');
        this.clients.delete(ws);
      });

      ws.on('error', (error) => {
        console.error('WebSocket error:', error);
        this.clients.delete(ws);
      });
    });

    console.log(`LSP Bridge server listening on localhost:${this.port}`);
  }

  async handleMessage(client, message) {
    const { type, language, data } = message;
    
    console.log('🔍 Bridge: Handling message type:', type, 'language:', language);

    switch (type) {
      case 'lsp/start':
        console.log('🚀 Bridge: Starting LSP for', language);
        await this.startLSP(client, language);
        break;
      case 'lsp/message':
        console.log('📤 Bridge: Forwarding LSP message for', language, 'method:', data?.method);
        await this.forwardToLSP(language, data);
        break;
      case 'lsp/stop':
        console.log('🛑 Bridge: Stopping LSP for', language);
        await this.stopLSP(language);
        break;
      default:
        console.warn('❌ Bridge: Unknown message type:', type);
    }
  }

  async startLSP(client, language) {
    console.log(`🚀 startLSP called for language: ${language}`);
    
    if (this.lspProcesses.has(language)) {
      console.log(`📋 LSP server already running for ${language}`);
      const existing = this.lspProcesses.get(language);
      existing.clients.add(client);
      this.sendMessage(client, {
        type: 'lsp/started',
        language,
        status: 'already_running'
      });
      return;
    }

    const lspCommand = this.getLSPCommand(language);
    console.log(`🔍 LSP command for ${language}:`, lspCommand);
    
    if (!lspCommand) {
      console.error(`❌ No LSP server configured for language: ${language}`);
      this.sendError(client, `No LSP server configured for language: ${language}`);
      return;
    }

    try {
      console.log(`🚀 Spawning LSP server: ${lspCommand.command} ${lspCommand.args.join(' ')}`);
      const process = spawn(lspCommand.command, lspCommand.args, {
        stdio: ['pipe', 'pipe', 'pipe']
      });
      
      console.log(`✅ LSP process spawned with PID: ${process.pid}`);

      const lspInfo = {
        process,
        clients: new Set([client]),
        language
      };

      this.lspProcesses.set(language, lspInfo);

      // Buffer for accumulating streaming JSON-RPC messages
      let messageBuffer = '';
      
      // Forward stdout from LSP to all clients
      process.stdout.on('data', (data) => {
        console.log(`📨 LSP ${language} stdout chunk:`, data.toString());
        
        // Accumulate data in buffer
        messageBuffer += data.toString();
        console.log(`📦 Buffer now contains ${messageBuffer.length} chars`);
        
        // Try to parse complete messages from buffer
        const parseResult = this.parseJSONRPCBuffer(messageBuffer);
        const messages = parseResult.messages;
        messageBuffer = parseResult.remaining;
        
        console.log(`📋 Parsed ${messages.length} complete JSON-RPC messages`);
        console.log(`📦 Buffer remaining: ${messageBuffer.length} chars`);
        
        messages.forEach(message => {
          console.log(`📤 Sending LSP response:`, JSON.stringify(message, null, 2));
          lspInfo.clients.forEach(client => {
            if (client.readyState === WebSocket.OPEN) {
              this.sendMessage(client, {
                type: 'lsp/message',
                language,
                data: message
              });
            }
          });
        });
      });

      process.stderr.on('data', (data) => {
        console.error(`❌ LSP ${language} stderr:`, data.toString());
      });

      process.on('exit', (code) => {
        console.log(`🛑 LSP ${language} exited with code ${code}`);
        this.lspProcesses.delete(language);
        lspInfo.clients.forEach(client => {
          if (client.readyState === WebSocket.OPEN) {
            this.sendMessage(client, {
              type: 'lsp/stopped',
              language,
              code
            });
          }
        });
      });

      process.on('error', (error) => {
        console.error(`💥 LSP ${language} process error:`, error);
        this.lspProcesses.delete(language);
      });

      this.sendMessage(client, {
        type: 'lsp/started',
        language,
        status: 'success'
      });

    } catch (error) {
      console.error(`Failed to start LSP for ${language}:`, error);
      this.sendError(client, `Failed to start LSP server for ${language}: ${error.message}`);
    }
  }

  async forwardToLSP(language, message) {
    const lspInfo = this.lspProcesses.get(language);
    if (!lspInfo) {
      console.warn(`No LSP process running for language: ${language}`);
      return;
    }

    try {
      const jsonrpc = JSON.stringify(message);
      const content = `Content-Length: ${Buffer.byteLength(jsonrpc)}\r\n\r\n${jsonrpc}`;
      lspInfo.process.stdin.write(content);
    } catch (error) {
      console.error(`Error forwarding message to LSP ${language}:`, error);
    }
  }

  async stopLSP(language) {
    const lspInfo = this.lspProcesses.get(language);
    if (!lspInfo) {
      return;
    }

    try {
      lspInfo.process.kill('SIGTERM');
      this.lspProcesses.delete(language);
      console.log(`Stopped LSP server for ${language}`);
    } catch (error) {
      console.error(`Error stopping LSP ${language}:`, error);
    }
  }

  getLSPCommand(language) {
    const commands = {
      javascript: { command: 'typescript-language-server', args: ['--stdio'] },
      typescript: { command: 'typescript-language-server', args: ['--stdio'] },
      python: { command: 'pylsp', args: [] },
      rust: { command: 'rust-analyzer', args: [] },
      clojure: { command: 'clojure-lsp', args: [] }
    };

    return commands[language];
  }

  parseJSONRPCBuffer(buffer) {
    const messages = [];
    let remaining = buffer;

    while (remaining.length > 0) {
      const headerEnd = remaining.indexOf('\r\n\r\n');
      if (headerEnd === -1) {
        // No complete header yet, keep everything in buffer
        break;
      }

      const headers = remaining.substring(0, headerEnd);
      const contentLengthMatch = headers.match(/Content-Length: (\d+)/);
      
      if (!contentLengthMatch) {
        console.error(`⚠️ No Content-Length header found in: ${headers}`);
        break;
      }

      const contentLength = parseInt(contentLengthMatch[1]);
      const messageStart = headerEnd + 4;
      const messageEnd = messageStart + contentLength;

      if (remaining.length < messageEnd) {
        // Incomplete message, keep everything in buffer
        console.log(`⚠️ Incomplete message: need ${messageEnd} bytes, have ${remaining.length}`);
        break;
      }

      const messageContent = remaining.substring(messageStart, messageEnd);
      
      try {
        const message = JSON.parse(messageContent);
        console.log(`✅ Parsed complete JSON-RPC message: ${message.method || 'response'}`);
        messages.push(message);
      } catch (error) {
        console.error('❌ Failed to parse JSON-RPC message:', error);
        console.error('❌ Message content was:', messageContent);
      }

      remaining = remaining.substring(messageEnd);
    }

    return {
      messages: messages,
      remaining: remaining
    };
  }

  parseJSONRPC(data) {
    const messages = [];
    let remaining = data;

    console.log(`🔍 parseJSONRPC input length: ${data.length}`);
    console.log(`🔍 parseJSONRPC raw data:`, JSON.stringify(data));

    while (remaining.length > 0) {
      const headerEnd = remaining.indexOf('\r\n\r\n');
      console.log(`🔍 Looking for header end, found at: ${headerEnd}`);
      
      if (headerEnd === -1) {
        console.log(`⚠️ No complete header found, breaking`);
        break;
      }

      const headers = remaining.substring(0, headerEnd);
      console.log(`🔍 Headers:`, headers);
      
      const contentLengthMatch = headers.match(/Content-Length: (\d+)/);
      
      if (!contentLengthMatch) {
        console.log(`⚠️ No Content-Length header found`);
        break;
      }

      const contentLength = parseInt(contentLengthMatch[1]);
      const messageStart = headerEnd + 4;
      const messageEnd = messageStart + contentLength;

      console.log(`🔍 Content-Length: ${contentLength}, messageStart: ${messageStart}, messageEnd: ${messageEnd}, remaining.length: ${remaining.length}`);

      if (remaining.length < messageEnd) {
        console.log(`⚠️ Incomplete message: need ${messageEnd} bytes, have ${remaining.length}`);
        break;
      }

      const messageContent = remaining.substring(messageStart, messageEnd);
      console.log(`🔍 Message content:`, messageContent);
      
      try {
        const message = JSON.parse(messageContent);
        console.log(`✅ Parsed JSON-RPC message:`, message);
        messages.push(message);
      } catch (error) {
        console.error('❌ Failed to parse JSON-RPC message:', error);
        console.error('❌ Message content was:', messageContent);
      }

      remaining = remaining.substring(messageEnd);
      console.log(`🔍 Remaining data length: ${remaining.length}`);
    }

    console.log(`✅ parseJSONRPC returning ${messages.length} messages`);
    return messages;
  }

  sendMessage(client, message) {
    if (client.readyState === WebSocket.OPEN) {
      client.send(JSON.stringify(message));
    }
  }

  sendError(client, error) {
    this.sendMessage(client, {
      type: 'error',
      message: error
    });
  }
}

// Start the bridge server
const bridge = new LSPBridge();

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('Shutting down LSP Bridge...');
  bridge.lspProcesses.forEach(async (lspInfo, language) => {
    await bridge.stopLSP(language);
  });
  process.exit(0);
});
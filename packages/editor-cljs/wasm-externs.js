/**
 * @fileoverview Externs for WASM module
 * @externs
 */

/** @type {!Object} */
var WasmEditorCore;

/** @constructor */
WasmEditorCore = function() {};

/** @param {string} content */
WasmEditorCore.prototype.init = function(content) {};

/** @return {string} */
WasmEditorCore.prototype.getText = function() {};

/** @return {number} */
WasmEditorCore.prototype.getLength = function() {};

/** @param {number} start @param {number} end @return {string} */
WasmEditorCore.prototype.getTextInRange = function(start, end) {};

/** @param {string} transaction @return {number} */
WasmEditorCore.prototype.applyTransaction = function(transaction) {};

/** @return {string} */
WasmEditorCore.prototype.getLastResult = function() {};

/** @return {string} */
WasmEditorCore.prototype.getLastErrorMessage = function() {};

WasmEditorCore.prototype.free = function() {};

/** @type {!Function} */
var wasm_bindgen;
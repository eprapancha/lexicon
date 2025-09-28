// Placeholder JavaScript grammar file
// This will be replaced with the actual tree-sitter JavaScript grammar
module.exports = grammar({
  name: 'javascript',

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.expression_statement,
      $.var_declaration,
      $.function_declaration
    ),

    expression_statement: $ => seq(
      $._expression,
      ';'
    ),

    var_declaration: $ => seq(
      choice('var', 'let', 'const'),
      $.identifier,
      optional(seq('=', $._expression)),
      ';'
    ),

    function_declaration: $ => seq(
      'function',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      $.block
    ),

    block: $ => seq(
      '{',
      repeat($._statement),
      '}'
    ),

    parameter_list: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier))
    ),

    _expression: $ => choice(
      $.identifier,
      $.number,
      $.string,
      $.call_expression
    ),

    call_expression: $ => seq(
      $.identifier,
      '(',
      optional($.argument_list),
      ')'
    ),

    argument_list: $ => seq(
      $._expression,
      repeat(seq(',', $._expression))
    ),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    number: $ => /\d+/,
    string: $ => /"[^"]*"/
  }
});
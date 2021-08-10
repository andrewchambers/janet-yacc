(import ../yacc)

(def c89-grammar
  ~(yacc
     (%start translation_unit)

     (primary_expression
       (:IDENTIFIER) _
       (:CONSTANT) _
       (:STRING_LITERAL) _
       (:LPAREN expression :RPAREN) _)

     (postfix_expression
       (primary_expression) _
       (postfix_expression :LBRACK expression :RBRACK) _
       (postfix_expression :LPAREN :RPAREN) _
       (postfix_expression :LPAREN argument_expression_list :RPAREN) _
       (postfix_expression :DOT_OP :IDENTIFIER) _
       (postfix_expression :PTR_OP :IDENTIFIER) _
       (postfix_expression :INC_OP) _
       (postfix_expression :DEC_OP) _)

     (argument_expression_list
       (assignment_expression) _
       (argument_expression_list :COMMA assignment_expression) _)

     (unary_expression
       (postfix_expression) _
       (:INC_OP unary_expression) _
       (:DEC_OP unary_expression) _
       (unary_operator cast_expression) _
       (:SIZEOF unary_expression) _
       (:SIZEOF :LPAREN type_name :RPAREN) _)

     (unary_operator
       (:AND_OP) _
       (:STAR_OP) _
       (:PLUS_OP) _
       (:MINUS_OP) _
       (:TILD_OP) _
       (:NOT_OP) _)

     (cast_expression
       (unary_expression) _
       (:LPAREN type_name :RPAREN cast_expression) _)

     (multiplicative_expression
       (cast_expression) _
       (multiplicative_expression :STAR_OP cast_expression) _
       (multiplicative_expression :DIV_OP cast_expression) _
       (multiplicative_expression :MOD_OP cast_expression) _)

     (additive_expression
       (multiplicative_expression) _
       (additive_expression :PLUS_OP multiplicative_expression) _
       (additive_expression :MINUS_OP multiplicative_expression) _)

     (shift_expression
       (additive_expression) _
       (shift_expression :LEFT_OP additive_expression) _
       (shift_expression :RIGHT_OP additive_expression) _)

     (relational_expression
       (shift_expression) _
       (relational_expression :LT_OP shift_expression) _
       (relational_expression :GT_OP shift_expression) _
       (relational_expression :LE_OP shift_expression) _
       (relational_expression :GE_OP shift_expression) _)

     (equality_expression
       (relational_expression) _
       (equality_expression :EQ_OP relational_expression) _
       (equality_expression :NE_OP relational_expression) _)

     (and_expression
       (equality_expression) _
       (and_expression :AND_OP equality_expression) _)

     (exclusive_or_expression
       (and_expression) _
       (exclusive_or_expression :XOR_OP and_expression) _)

     (inclusive_or_expression
       (exclusive_or_expression) _
       (inclusive_or_expression :OR_OP exclusive_or_expression) _)

     (logical_and_expression
       (inclusive_or_expression) _
       (logical_and_expression :AND_AND_OP inclusive_or_expression) _)

     (logical_or_expression
       (logical_and_expression) _
       (logical_or_expression :OR_OP logical_and_expression) _)

     (conditional_expression
       (logical_or_expression) _
       (logical_or_expression :TERN_OP expression :COLON_OP conditional_expression) _)

     (assignment_expression
       (conditional_expression) _
       (unary_expression assignment_operator assignment_expression) _)

     (assignment_operator
       (:ASSIGN) _
       (:MUL_ASSIGN) _
       (:DIV_ASSIGN) _
       (:MOD_ASSIGN) _
       (:ADD_ASSIGN) _
       (:SUB_ASSIGN) _
       (:LEFT_ASSIGN) _
       (:RIGHT_ASSIGN) _
       (:AND_ASSIGN) _
       (:XOR_ASSIGN) _
       (:OR_ASSIGN) _)

     (expression
       (assignment_expression) _
       (expression :COMMA assignment_expression) _)

     (constant_expression
       (conditional_expression) _)

     (declaration
       (declaration_specifiers :SEMICOLON) _
       (declaration_specifiers init_declarator_list :SEMICOLON) _)

     (declaration_specifiers
       (storage_class_specifier) _
       (storage_class_specifier declaration_specifiers) _
       (type_specifier) _
       (type_specifier declaration_specifiers) _
       (type_qualifier) _
       (type_qualifier declaration_specifiers) _)

     (init_declarator_list
       (init_declarator) _
       (init_declarator_list :COMMA init_declarator) _)

     (init_declarator
       (declarator) _
       (declarator :ASSIGN initializer) _)

     (storage_class_specifier
       (:TYPEDEF) _
       (:EXTERN) _
       (:STATIC) _
       (:AUTO) _
       (:REGISTER) _)

     (type_specifier
       (:VOID) _
       (:CHAR) _
       (:SHORT) _
       (:INT) _
       (:LONG) _
       (:FLOAT) _
       (:DOUBLE) _
       (:SIGNED) _
       (:UNSIGNED) _
       (struct_or_union_specifier) _
       (enum_specifier) _
       (:TYPE_NAME) _)

     (struct_or_union_specifier
       (struct_or_union :IDENTIFIER :LBRACE struct_declaration_list :RBRACE) _
       (struct_or_union :LBRACE struct_declaration_list :RBRACE) _
       (struct_or_union :IDENTIFIER) _)

     (struct_or_union
       (:STRUCT) _
       (:UNION) _)

     (struct_declaration_list
       (struct_declaration) _
       (struct_declaration_list struct_declaration) _)

     (struct_declaration
       (specifier_qualifier_list struct_declarator_list :SEMICOLON) _)

     (specifier_qualifier_list
       (type_specifier specifier_qualifier_list) _
       (type_specifier) _
       (type_qualifier specifier_qualifier_list) _
       (type_qualifier) _)

     (struct_declarator_list
       (struct_declarator) _
       (struct_declarator_list :COMMA struct_declarator) _)

     (struct_declarator
       (declarator) _
       (:COLON_OP constant_expression) _
       (declarator :COLON_OP constant_expression) _)

     (enum_specifier
       (:ENUM :LBRACE enumerator_list :RBRACE) _
       (:ENUM :IDENTIFIER :LBRACE enumerator_list :RBRACE) _
       (:ENUM :IDENTIFIER) _)

     (enumerator_list
       (enumerator) _
       (enumerator_list :COMMA enumerator) _)

     (enumerator
       (:IDENTIFIER) _
       (:IDENTIFIER :ASSIGN constant_expression) _)

     (type_qualifier
       (:CONST) _
       (:VOLATILE) _)

     (declarator
       (pointer direct_declarator) _
       (direct_declarator) _)

     (direct_declarator
       (:IDENTIFIER) _
       (:LPAREN declarator :RPAREN) _
       (direct_declarator :LBRACK constant_expression :RBRACK) _
       (direct_declarator :LBRACK :RBRACK) _
       (direct_declarator :LPAREN parameter_type_list :RPAREN) _
       (direct_declarator :LPAREN identifier_list :RPAREN) _
       (direct_declarator :LPAREN :RPAREN) _)

     (pointer
       (:STAR_OP) _
       (:STAR_OP type_qualifier_list) _
       (:STAR_OP pointer) _
       (:STAR_OP type_qualifier_list pointer) _)

     (type_qualifier_list
       (type_qualifier) _
       (type_qualifier_list type_qualifier) _)

     (parameter_type_list
       (parameter_list) _
       (parameter_list :COMMA :ELLIPSIS) _)

     (parameter_list
       (parameter_declaration) _
       (parameter_list :COMMA parameter_declaration) _)

     (parameter_declaration
       (declaration_specifiers declarator) _
       (declaration_specifiers abstract_declarator) _
       (declaration_specifiers) _)

     (identifier_list
       (:IDENTIFIER) _
       (identifier_list :COMMA :IDENTIFIER) _)

     (type_name
       (specifier_qualifier_list) _
       (specifier_qualifier_list abstract_declarator) _)

     (abstract_declarator
       (pointer) _
       (direct_abstract_declarator) _
       (pointer direct_abstract_declarator) _)

     (direct_abstract_declarator
       (:LPAREN abstract_declarator :RPAREN) _
       (:LBRACK :RBRACK) _
       (:LBRACK constant_expression :RBRACK) _
       (direct_abstract_declarator :LBRACK :RBRACK) _
       (direct_abstract_declarator :LBRACK constant_expression :RBRACK) _
       (:LPAREN :RPAREN) _
       (:LPAREN parameter_type_list :RPAREN) _
       (direct_abstract_declarator :LPAREN :RPAREN) _
       (direct_abstract_declarator :LPAREN parameter_type_list :RPAREN) _)

     (initializer
       (assignment_expression) _
       (:LBRACE initializer_list :RBRACE) _
       (:LBRACE initializer_list :COMMA :RBRACE) _)

     (initializer_list
       (initializer) _
       (initializer_list :COMMA initializer) _)

     (statement
       (labeled_statement) _
       (compound_statement) _
       (expression_statement) _
       (selection_statement) _
       (iteration_statement) _
       (jump_statement) _)

     (labeled_statement
       (:IDENTIFIER :COLON_OP statement) _
       (:CASE constant_expression :COLON_OP statement) _
       (:DEFAULT :COLON_OP statement) _)

     (compound_statement
       (:LBRACE :RBRACE) _
       (:LBRACE statement_list :RBRACE) _
       (:LBRACE declaration_list :RBRACE) _
       (:LBRACE declaration_list statement_list :RBRACE) _)

     (declaration_list
       (declaration) _
       (declaration_list declaration) _)

     (statement_list
       (statement) _
       (statement_list statement) _)

     (expression_statement
       (:SEMICOLON) _
       (expression :SEMICOLON) _)

     (selection_statement
       (:IF :LPAREN expression :RPAREN statement) _
       (:IF :LPAREN expression :RPAREN statement :ELSE statement) _
       (:SWITCH :LPAREN expression :RPAREN statement) _)

     (iteration_statement
       (:WHILE :LPAREN expression :RPAREN statement) _
       (:DO statement :WHILE :LPAREN expression :RPAREN :SEMICOLON) _
       (:FOR :LPAREN expression_statement expression_statement :RPAREN statement) _
       (:FOR :LPAREN expression_statement expression_statement expression :RPAREN statement) _)

     (jump_statement
       (:GOTO :IDENTIFIER :SEMICOLON) _
       (:CONTINUE :SEMICOLON) _
       (:BREAK :SEMICOLON) _
       (:RETURN :SEMICOLON) _
       (:RETURN expression :SEMICOLON) _)

     (translation_unit
       (external_declaration) _
       (translation_unit external_declaration) _)

     (external_declaration
       (function_definition) _
       (declaration) _)

     (function_definition
       (declaration_specifiers declarator declaration_list compound_statement) _
       (declaration_specifiers declarator compound_statement) _
       (declarator declaration_list compound_statement) _
       (declarator compound_statement) _)))

(def dbg-output @"")

(def yacc-tables 
  (with-dyns [:yydebug dbg-output]
    (yacc/compile c89-grammar)))

#(print dbg-output)
// Grammar for SymEngine printer output, used by src/seFromSE.c to translate
// symengine syntax back into rxode2/C syntax.
//
// This replaces the recursive R walker .rxFromSE() (R/symengine.R), which
// re-parsed every symengine string with R's own parse() and then emitted with
// nested paste0() and a 9-deep sub() regex chain.  rxFromSE() is ~90% of all
// symbolic-derivative time while symengine::D() itself is ~0.5%.
//
// Regenerate src/seFromSE.g.d_parser.h with .rxodeBuildCode() (R/build.R).
//
// Written with explicit left/right recursion rather than EBNF repetition on
// purpose: dparser expands ( )* and ( )? into synthetic "rule__N" productions,
// and the emitter would then have to walk through nodes that carry no meaning.
// Spelling the recursion out keeps every node's arity fixed and the C walk a
// straight switch on the node name.
//
// Only what SymEngine's printer can emit is accepted.  Note it prints power as
// '^', puts a space after a unary minus inside parentheses ("a^(- 1)"), and
// prints exact rationals as "1/2".

translation_unit : expression ;

expression : add_expression ;

// left recursive: '+' and '-' are left associative
add_expression
  : add_expression '+' mul_expression
  | add_expression '-' mul_expression
  | mul_expression
  ;

mul_expression
  : mul_expression '*' unary_expression
  | mul_expression '/' unary_expression
  | unary_expression
  ;

unary_expression
  : '-' unary_expression
  | '+' unary_expression
  | power_expression
  ;

// right recursive through unary_expression: '^' is right associative and the
// exponent may be signed (x^-1, x^(- 1))
power_expression
  : primary_expression '**' unary_expression
  | primary_expression '^' unary_expression
  | primary_expression
  ;

primary_expression
  : function_call
  | number
  | symbol
  | '(' expression ')'
  ;

function_call
  : function_name '(' arg_list ')'
  | function_name '(' ')'
  ;

arg_list
  : arg_list ',' expression
  | expression
  ;

function_name : identifier ;

symbol : identifier ;

// float_num/integer_num rather than float/integer: the walker tests node
// kinds with seNodeHas(), which stringifies the production name and uses it as
// a seNodeInfo field, and `float` is a C keyword
number : float_num | integer_num ;

// symengine symbols carry rxode2's mangled names: THETA_1_, ETA_2_,
// rx__d_dt_depot__, rx__df_a_dy_b__, rx__sens_a_BY_p__, rx_f_depot_,
// and user names with dots such as eta.ka
identifier: "[a-zA-Z_][a-zA-Z0-9_.]*" $term -1;

integer_num: "[0-9]+" $term -1;

float_num: "([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][\-\+]?[0-9]+)?|[0-9]+[eE][\-\+]?[0-9]+" $term -2;

// Grammar for rxode2 expression syntax, used by src/rxToSE.c to translate
// rxode2 into the text symengine parses.
//
// The companion of inst/seFromSE.g.  Same reasoning: rxToSE() is the second
// half of the R text translation that dominates symbolic-derivative setup, and
// it walks R's own parse tree through ~30 special-case handlers.
//
// Regenerate src/rxToSE.g.d_parser.h with .rxodeBuildCode() (R/build.R).
//
// Written with explicit left/right recursion rather than EBNF repetition:
// dparser expands ( )* into synthetic "rule__N" productions that carry no
// meaning for the walk.  Production names double as seNodeInfo fields in the
// emitter, so none of them may be a C keyword -- hence float_num/integer_num.

translation_unit : expression ;

expression : or_expression ;

or_expression
  : or_expression '||' and_expression
  | or_expression '|' and_expression
  | and_expression
  ;

and_expression
  : and_expression '&&' rel_expression
  | and_expression '&' rel_expression
  | rel_expression
  ;

rel_expression
  : rel_expression rel_op add_expression
  | add_expression
  ;

rel_op : '==' | '!=' | '<=' | '>=' | '<' | '>' ;

add_expression
  : add_expression '+' mul_expression
  | add_expression '-' mul_expression
  | mul_expression
  ;

mul_expression
  : mul_expression '*' unary_expression
  | mul_expression '/' unary_expression
  | mul_expression '%%' unary_expression
  | unary_expression
  ;

unary_expression
  : '-' unary_expression
  | '+' unary_expression
  | '!' unary_expression
  | power_expression
  ;

// right associative through unary_expression, so a^-b parses
power_expression
  : primary_expression '**' unary_expression
  | primary_expression '^' unary_expression
  | primary_expression
  ;

primary_expression
  : function_call
  | index_expression
  | number
  | symbol
  | '(' expression ')'
  ;

// THETA[1] / ETA[2]
index_expression : identifier '[' expression ']' ;

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

number : float_num | integer_num ;

identifier: "[a-zA-Z_.][a-zA-Z0-9_.]*" $term -1;

integer_num: "[0-9]+" $term -1;

float_num: "([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][\-\+]?[0-9]+)?|[0-9]+[eE][\-\+]?[0-9]+" $term -2;

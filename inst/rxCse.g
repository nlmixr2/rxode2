// Grammar for ONE normalized rxode2 statement, used by src/rxCse.c to do
// common-subexpression elimination in C.
//
// The third grammar in this family, after inst/seFromSE.g and inst/rxToSE.g,
// and it shares their reasoning: rxOptExpr()'s search is O(k^2) in the number
// of distinct subexpressions because it counts them in a named R list, and on a
// second-order sensitivity model k is enormous.
//
// The start symbol is a SINGLE statement, not a statement list.  The input is
// rxNorm() output, which is one statement per line, so the driver splits on
// newlines and parses each line independently -- that is what lets the counting
// pass run per line in an OpenMP region, and it means this grammar never has to
// disambiguate statement separators.
//
// There is deliberately NO if/else here.  Everything that reaches rxOptExpr()
// has already been through rxPrune(), which rewrites conditionals into
// arithmetic, so a conditional is a decline rather than something to support.
// (..rxOpt() has no if/else branch either and renders one as `if(c, a, b)`,
// which does not re-parse.)
//
// Regenerate src/rxCse.g.d_parser.h with .rxodeBuildCode() (R/build.R).
//
// Written with explicit left/right recursion rather than EBNF repetition:
// dparser expands ( )* into synthetic "rule__N" productions that carry no
// meaning for the walk.  Production names double as csNodeInfo fields in the
// emitter, so none of them may be a C keyword -- hence float_num/integer_num.

translation_unit : statement ;

// `lhs = rhs`, `lhs <- rhs`, `lhs ~ rhs`, or a bare call such as `dvid(3, 4)`
// that is passed through untouched.  The trailing ';' is what rxNorm() emits.
statement
  : lhs assign_op expression end_statement
  | expression end_statement
  ;

assign_op : '=' | '<-' | '~' ;

end_statement : ';' | ;

// The set ..rxOptLhs() accepts (R/rxOptExpr.R:160-202).  The heads are not
// enumerated here: a generic call is accepted and the emitter declines on any
// head outside the set, the same way rxToSE's index_expression accepts any
// identifier and the emitter restricts it to THETA/ETA.
lhs
  : lhs '/' lhs_primary
  | lhs_primary
  ;

lhs_primary
  : identifier '(' lhs_args ')'
  | '(' lhs ')'
  | identifier
  ;

lhs_args
  : lhs_args ',' expression
  | expression
  ;

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

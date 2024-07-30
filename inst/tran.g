//loop
statement_list : (statement)+ ;

statement 
  : assignment end_statement
  | ini        end_statement
  | ini0       end_statement
  | ini0f      end_statement
  | fbio       end_statement
  | alag       end_statement
  | rate       end_statement
  | dur        end_statement 
  | derivative end_statement
  | dfdy       end_statement
  | mtime      end_statement
  | mat0       end_statement
  | matF       end_statement
  | printf_statement end_statement
  | param_statement end_statement
  | cmt_statement end_statement
  | dvid_statementI end_statement
  | break_statement end_statement
  | simfun_statement end_statement
  | compound_statement
  | selection_statement
  | ifelse_statement
  | end_statement ;


compound_statement : '{' statement_list? '}' ;

ifelse_statement
   : 'ifelse' '(' logical_or_expression ','  statement ',' statement ')' end_statement;

selection_statement
  :   "(if|while)" '(' logical_or_expression ')' statement ('else' statement)?;

break_statement
    : 'break';

simfun_statement : "(simeps|simeta)" '(' ')' ;

cmt_statement
    : 'cmt' '(' identifier_r_no_output ')';

param_statement
    : "params?" '(' (identifier_r | theta0 | theta | eta) (',' (identifier_r | theta0 | theta | eta) )*  ')';

printf_statement
  : printf_command '(' string (',' logical_or_expression )* ')';

printf_command
  : 'printf' | 'Rprintf' | 'print';

dvid_statementI
  : 'dvid' '(' decimalintN (',' decimalintN )* ')';

decimalintN
  : '-'? decimalint;
ini0      : identifier_r '(0)' ('=' | '<-' ) ini_const;

ini0f     : identifier_r '(0)' ('=' | '<-' ) logical_or_expression;

ini        : identifier_r ('=' | '<-' ) ini_const;

derivative : 'd/dt' '(' identifier_r_no_output ')' ('=' | '<-' | '~') ('+' | '-' | ) logical_or_expression;
der_rhs    : 'd/dt' '(' identifier_r_no_output ')';

dfdy        : 'df' '(' identifier_r_no_output ')/dy(' (theta0_noout | theta_noout | eta_noout | identifier_r_no_output) ')' ('=' | '<-' ) logical_or_expression;
dfdy_rhs    : 'df' '(' identifier_r_no_output ')/dy(' (theta0_noout | theta_noout | eta_noout | identifier_r_no_output) ')';

fbio        : ('f' | 'F')  '(' identifier_r_no_output ')' ('=' | '<-' | '~' ) logical_or_expression;
alag        : ('alag' | 'lag')  '(' identifier_r_no_output ')' ('=' | '<-' | '~' ) logical_or_expression;
rate        : 'rate'  '(' identifier_r_no_output ')' ('=' | '<-' | '~' ) logical_or_expression;
dur        : 'dur'  '(' identifier_r_no_output ')' ('=' | '<-' | '~' ) logical_or_expression;



end_statement : (';')* ;

assignment : identifier_r  ('=' | '<-' | '~' ) logical_or_expression;

mat0: '_rxM' '=' logical_or_expression;

matF: '_rxF' '=' logical_or_expression;

mtime     : 'mtime' '(' identifier_r_no_output ')' ('=' | '<-' | '~') logical_or_expression;

logical_or_expression : logical_and_expression 
    (('||' | '|')  logical_and_expression)* ;

logical_and_expression : equality_expression0 
    (('&&' | '&') equality_expression0)* ;

equality_expression0 : equality_expression |
    '(' equality_expression ')' |
    '!' '(' equality_expression ')' |
     equality_str |
    '(' equality_str ')' |
    '!' '(' equality_str ')' |
    '(' '!' identifier_r ')' |
    '!' identifier_r | 
    '!' function ;

equality_str : equality_str1 | equality_str2;
equality_str1 : string ('!=' | '==' ) identifier_r;
equality_str2 : identifier_r ('!=' | '==' ) string;

equality_expression : relational_expression 
    (('!=' | '==' ) relational_expression)* ;

relational_expression : additive_expression
    (('<' | '>' | '<=' | '>=') additive_expression)* ;

additive_expression : multiplicative_expression
    (('+' | '-') multiplicative_expression)* ;

multiplicative_expression : unary_expression 
    (mult_part)* ;

mult_part : ('*' | '/') unary_expression ;

unary_expression : ('+' | '-')? (theta0 | theta | eta | primary_expression | power_expression );

exponent_expression : ('+' | '-')? (theta0 | theta | eta | primary_expression );

power_expression : primary_expression power_operator exponent_expression;

power_operator   : ('^' | '**');

primary_expression 
  : constant
  | identifier_r
  | theta0
  | theta
  | eta
  | der_rhs
  | dfdy_rhs
  | function
  | ifelse
  | '(' logical_or_expression ')'
  ;

ifelse : 'ifelse' '(' logical_or_expression ',' logical_or_expression ',' logical_or_expression ')' ;

function : function_name '(' (logical_or_expression)* (',' logical_or_expression)* ')' ;

function_name: identifier | 'lag';

ini_const : '-'? constant;
trans_const: identifier_r | '-'? constant;

constant : decimalint | float1 | float2;

identifier_r: identifier_r_extra | identifier_r_1 | identifier_r_2 ;

identifier_r_no_output: identifier_r_no_output_1 | identifier_r_no_output_2 | identifier_r_extra;

identifier_r_extra: 'alag' | 'f'| 'F' | 'rate' | 'dur' | 'lag';

theta: ('THETA' | 'theta') '[' decimalintNo0 ']';
eta: ('ETA' | 'eta') '[' decimalintNo0 ']';
theta0: ('THETA' | 'theta' | 'ETA' | 'eta');

theta_noout: ('THETA' | 'theta') '[' decimalintNo0 ']';
eta_noout: ('ETA' | 'eta') '[' decimalintNo0 ']';
theta0_noout: ('THETA' | 'theta' | 'ETA' | 'eta');


decimalintNo0: "([1-9][0-9]*)" $term -1;
decimalint: "0|([1-9][0-9]*)" $term -1;
string: "\"([^\"\\]|\\[^])*\"";
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier_r_1: "[a-zA-Z][a-zA-Z0-9_.]*" $term -4;
identifier_r_no_output_1: "[a-zA-Z][a-zA-Z0-9_.]*" $term -4;
identifier_r_2: "[.]+[a-zA-Z_][a-zA-Z0-9_.]*" $term -4;
identifier_r_no_output_2: "[.]+[a-zA-Z_][a-zA-Z0-9_.]*" $term -4;
identifier: "[a-zA-Z][a-zA-Z0-9_.]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: '#' "[^\n]*";


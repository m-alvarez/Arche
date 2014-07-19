%{
    open Ast
%}

%token <string> Id
%token <int> Int
%token EXTEND WITH NEW FUNCTION DELAY IS END
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET MINUS PLUS MUL DIV
%token COMMA COLON SEMICOLON
%left MINUS
%left PLUS
%left MUL
%left DIV
%token EOF
%nonassoc WITH
%nonassoc NEW
%nonassoc DOT
%nonassoc Id
%token DOT ARROW 
%nonassoc LPAR RPAR
%start pgm
%type <Ast.expression> pgm


%%

pgm:
    expr EOF { $1 }
;

expr:
    | literal                  { Lit($1) }
    | Id                       { Name($1) }
    | NEW expr                 { New($2) }
    | LPAR expr RPAR           { $2 }
    | EXTEND expr WITH expr    { Extend($2, $4) }
    | expr LPAR expr_list RPAR { Apply($1, $3) }
    | expr DOT Id              { Method($1, $3) }
    | expr op expr             { Bin_op($2, $1, $3) }
;

op:
    | PLUS  { Plus }
    | MINUS { Minus }
    | MUL   { Mul }
    | DIV   { Div }
;

expr_list:
    | expr COMMA expr_list { $1 :: $3 }
    |                      { [] }
;

literal:
    | LBRACE fields RBRACE                       { Obj($2) }
    | FUNCTION LPAR arg_list RPAR ARROW expr END { Fn($3, $6) }
    | Int                                        { Int($1) }
;

arg_list:
    | { [] }
    | Id { [$1] }
    | Id COMMA arg_list { $1 :: $3 } 
;

field:
    | Id pot IS decl SEMICOLON { ($1, { $4 with pot = $2 }) } 
;

decl:
    | expr                    { {self=None; pot=0; body=$1} }
    | LPAR Id RPAR expr { {self=Some($2); pot=0; body=$4} }
;

pot:
    |                       { 0 }
    | LBRACKET Int RBRACKET { 
        if $2 >= 0 
        then $2 
        else raise (Failure "Potency must be positive")
    }
;

fields:
    |              { [] }
    | field fields { $1 :: $2 }
;
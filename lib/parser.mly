%{
open Ast
%}

%token <string> IDENT
%token <int> INT
%token <string> STRING
%token TRUE FALSE
%token ASSIGN PLUS MINUS EQ NOT_EQ
%token BANG ASTERISK SLASH LT GT
%token COMMA SEMICOLON COLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token FUNCTION LET RETURN IF ELSE
%token EOF

%right BANG
%nonassoc LT GT EQ NOT_EQ
%left PLUS MINUS
%left ASTERISK SLASH
%right LPAREN
%right LBRACK

%start <program> program
%%

program: statement_list EOF { $1 }

block_statement:
  | LBRACE statement_list RBRACE
    { $2 }

statement_list:
  | separated_list(SEMICOLON, statement) { $1 }

statement:
  | LET identifier ASSIGN expression  { Let ($2, $4) }
  | RETURN expression                 { Return $2 }
  | expression                        { Expression $1 }

expression:
  | literal                                     { Literal $1 }
  | identifier                                  { Identifier $1 }
  | arry                                        { $1 }
  | hash                                        { $1 }
  | index                                       { $1 }
  | prefix_operator expression                  { Prefix ($1, $2) }
  | expression infix_operator expression        { Infix ($2, $1, $3) }
  | if_expression                               { $1 }
  | function_expression                         { $1 }
  | call_expression                             { $1 }
  | LPAREN expression RPAREN                    { $2 }

arry:
  | LBRACK separated_list(COMMA, expression) RBRACK
    { Arry $2 }

hash:
  | LBRACE separated_list(COMMA, expression_pair) RBRACE
    { Hash $2 }

index:
  | expression LBRACK expression RBRACK
    { Index ($1, $3) }

expression_pair:
  | expression COLON expression { ($1, $3) }

%inline prefix_operator:
  | BANG      { Not }
  | MINUS     { Negate }

%inline infix_operator:
  | EQ        { Equal }
  | NOT_EQ    { NotEqual }
  | LT        { LessThan }
  | GT        { GreaterThan }
  | PLUS      { Plus }
  | MINUS     { Minus }
  | ASTERISK  { Multiply }
  | SLASH     { Divide }

if_expression:
  | IF LPAREN expression RPAREN block_statement
    { If ($3, $5, None) }
  | IF LPAREN expression RPAREN block_statement ELSE block_statement
    { If ($3, $5, Some $7) }

function_expression:
  | FUNCTION LPAREN separated_list(COMMA, identifier) RPAREN block_statement
    { Function ($3, $5) }

call_expression:
  | expression LPAREN separated_list(COMMA, expression) RPAREN
    { Call ($1, $3) }

identifier:
  | IDENT     { $1 }

literal:
  | INT       { LInteger $1 }
  | TRUE      { LBoolean true }
  | FALSE     { LBoolean false }
  | STRING    { LString $1 }

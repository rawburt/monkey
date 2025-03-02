type identifier = string

type prefix_operator =
  | Not
  | Negate

type infix_operator =
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | Plus
  | Minus
  | Multiply
  | Divide

type literal =
  | LInteger of int
  | LBoolean of bool
  | LString of string

type expression =
  | Literal of literal
  | Identifier of identifier
  | Arry of expression list
  | Hash of (expression * expression) list
  | Index of expression * expression
  | Prefix of prefix_operator * expression
  | Infix of infix_operator * expression * expression
  | If of expression * block_statement * block_statement option
  | Function of identifier list * block_statement
  | Call of expression * expression list

and statement =
  | Let of identifier * expression
  | Return of expression
  | Expression of expression

and block_statement = statement list

type program = block_statement

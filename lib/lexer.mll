{
  open Parser
  open Lexing

  let keywords =
    [
      ("fn", FUNCTION);
      ("let", LET);
      ("true", TRUE);
      ("false", FALSE);
      ("if", IF);
      ("else", ELSE);
      ("return", RETURN);
    ]

  let kw_or_ident lexeme =
    match List.assoc_opt lexeme keywords with
    | Some token -> token
    | None -> IDENT lexeme

  exception SyntaxError of string
}

let ident = ['a'-'z' 'A'-'Z' '_']+
let int = ['0'-'9']+
let ws = [' ' '\t' '\r']+

rule token = parse
  | ws   { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | '='  { ASSIGN }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '!'  { BANG }
  | '*'  { ASTERISK }
  | '/'  { SLASH }
  | '<'  { LT }
  | '>'  { GT }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | '['  { LBRACK }
  | ']'  { RBRACK }
  | ','  { COMMA }
  | ';'  { SEMICOLON }
  | ':'  { COLON }
  | "==" { EQ }
  | "!=" { NOT_EQ }
  | ident as i  { kw_or_ident i }
  | int as i    { INT (int_of_string i) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | eof { EOF }
  | _   { raise (SyntaxError ("unexpected character: " ^ (lexeme lexbuf))) }

and read_string buf = parse
  | '"'   { STRING (Buffer.contents buf) }
  | '\n'  { new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof   { raise (SyntaxError "string not terminated") }

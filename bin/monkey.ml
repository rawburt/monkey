open Monkey

let pos lexpos =
  let file = lexpos.Lexing.pos_fname in
  let line = lexpos.Lexing.pos_lnum in
  let column = lexpos.Lexing.pos_cnum - lexpos.Lexing.pos_bol + 1 in
  (file, line, column)

let error_msg lexbuf kind msg =
  let file, line, column = pos lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%s (file \"%s\", line %d, column %d): %s" kind file line
    column msg

let parse_and_run env lexbuf =
  try
    let program = Parser.program Lexer.token lexbuf in
    let result = Evaluator.eval env program in
    Ok result
  with
  | Lexer.SyntaxError msg ->
      Error (error_msg lexbuf "Syntax error" msg)
  | Parser.Error ->
      Error
        (error_msg lexbuf "Parse error"
           ("unexpected token: " ^ Lexing.lexeme lexbuf))

let run_repl env =
  print_endline "Monkey programming language v0.1.ocaml";
  let rec loop i =
    print_string ("(" ^ string_of_int i ^ ")>> ");
    flush_all ();
    let input = input_line stdin in
    let lexbuf = Lexing.from_string input in
    Lexing.set_filename lexbuf ("REPL:" ^ string_of_int i);
    (try
       match parse_and_run env lexbuf with
       | Ok obj ->
           print_endline (Obj.string_of_obj obj)
       | Error err ->
           print_endline err
     with
    | Evaluator.EvalError msg ->
        print_endline ("Runtime error. " ^ msg));
    loop (i + 1)
  in
  loop 0

let eval_file env file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf file;
  (match parse_and_run env lexbuf with
  | Ok _ ->
      ()
  | Error err ->
      print_endline (file ^ ": " ^ err));
  close_in ic

let _ =
  let usage_msg = "monkey: [-repl] [files ..]" in
  let repl = ref false in
  let files = ref [] in
  let filefn f = files := f :: !files in
  let speclist = [ ("-repl", Arg.Set repl, "Run a REPL session") ] in
  let env = Environment.create () in
  Arg.parse speclist filefn usage_msg;
  List.iter (eval_file env) !files;
  if !repl then run_repl env

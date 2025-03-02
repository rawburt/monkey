open Ast
open Obj

exception EvalReturn of Obj.t
exception EvalError of string

let error msg = raise (EvalError msg)

let builtins =
  [
    ("len", Builtin.len);
    ("first", Builtin.first);
    ("last", Builtin.last);
    ("rest", Builtin.rest);
    ("push", Builtin.push);
    ("puts", Builtin.puts);
  ]

let rec eval_literal = function
  | LInteger i ->
      OInteger i
  | LBoolean b ->
      if b then true_obj else false_obj
  | LString s ->
      OString s

and eval_prefix prefix_operator right =
  match prefix_operator with
  | Not ->
      if right = true_obj then false_obj
      else if right = false_obj then true_obj
      else if right = null_obj then true_obj
      else false_obj
  | Negate -> (
      match right with
      | OInteger i ->
          OInteger (-i)
      | _ ->
          error "invalid object for minus operator")

and eval_integer_infix_expression infix_operator left right =
  match infix_operator with
  | Plus ->
      OInteger (left + right)
  | Minus ->
      OInteger (left - right)
  | Multiply ->
      OInteger (left * right)
  | Divide ->
      OInteger (left / right)
  | LessThan ->
      obj_of_bool (left < right)
  | GreaterThan ->
      obj_of_bool (left > right)
  | Equal ->
      obj_of_bool (left = right)
  | NotEqual ->
      obj_of_bool (left <> right)

and eval_boolean_infix_expression infix_operator left right =
  match infix_operator with
  | Equal ->
      obj_of_bool (left = right)
  | NotEqual ->
      obj_of_bool (left <> right)
  | _ ->
      error "invalid objects for boolean infix operator"

and eval_if_expression env cond then_block else_block =
  if is_truthy (eval_expression env cond) then
    eval_block_statement env then_block
  else
    match else_block with
    | Some block ->
        eval_block_statement env block
    | None ->
        null_obj

and apply_function fn args =
  match fn with
  | OFunction fn ->
      let env = Environment.create ~outer:fn.fn_env () in
      List.iter2 (fun p a -> Environment.set env p a) fn.fn_params args;
      eval env fn.fn_body
  | OBuiltin f -> (
      match f args with
      | Ok result ->
          result
      | Error err ->
          error err)
  | _ ->
      error "not a function"

and eval_identifier env identifier =
  match Environment.get env identifier with
  | Some obj ->
      obj
  | None -> (
      match List.assoc_opt identifier builtins with
      | Some f ->
          OBuiltin f
      | None ->
          error ("not found: " ^ identifier))

and eval_index left index =
  match left with
  | OArray arry -> (
      match index with
      | OInteger i -> (
          match List.nth_opt arry i with
          | Some o ->
              o
          | None ->
              error "index out of range")
      | _ ->
          error "not an index")
  | OHash hash -> (
      match Hashtbl.find_opt hash index with
      | Some obj ->
          obj
      | None ->
          error ("key not found: " ^ string_of_obj index))
  | _ ->
      error "not an array"

and eval_infix infix_operator left right =
  if type_of left <> type_of right then error "type mismatch"
  else
    match (left, right) with
    | OInteger l, OInteger r ->
        eval_integer_infix_expression infix_operator l r
    | OBoolean l, OBoolean r ->
        eval_boolean_infix_expression infix_operator l r
    | OString l, OString r ->
        OString (l ^ r)
    | _ ->
        failwith "invalid infix expression"

and eval_expression env = function
  | Literal literal ->
      eval_literal literal
  | Identifier identifier ->
      eval_identifier env identifier
  | Arry expressions ->
      let objects = List.map (eval_expression env) expressions in
      OArray objects
  | Hash expression_pairs ->
      let pairs =
        List.map
          (fun (k, v) -> (eval_expression env k, eval_expression env v))
          expression_pairs
      in
      let hash = Hashtbl.create 22 in
      List.iter (fun (k, v) -> Hashtbl.replace hash k v) pairs;
      OHash hash
  | Index (left_expression, index_expression) ->
      let left = eval_expression env left_expression in
      let index = eval_expression env index_expression in
      eval_index left index
  | Prefix (prefix_operator, expression) ->
      let right = eval_expression env expression in
      eval_prefix prefix_operator right
  | Infix (infix_operator, lexpression, rexpression) ->
      let left = eval_expression env lexpression in
      let right = eval_expression env rexpression in
      eval_infix infix_operator left right
  | If (cond, then_block, else_block) ->
      eval_if_expression env cond then_block else_block
  | Function (params, body) ->
      let fn = { fn_params = params; fn_body = body; fn_env = env } in
      OFunction fn
  | Call (fn_expression, arg_expressions) ->
      let fn = eval_expression env fn_expression in
      let args = List.map (eval_expression env) arg_expressions in
      apply_function fn args

and eval_statement env = function
  | Return expression ->
      raise (EvalReturn (eval_expression env expression))
  | Expression expression ->
      eval_expression env expression
  | Let (identifier, expression) ->
      let obj = eval_expression env expression in
      Environment.set env identifier obj;
      obj

and eval_block_statement env block_statement =
  let result = ref ONull in
  List.iter (fun s -> result := eval_statement env s) block_statement;
  !result

and eval_statements env statements =
  try eval_block_statement env statements with
  | EvalReturn result ->
      result

and eval env program = eval_statements env program

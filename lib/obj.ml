type obj_type =
  | NULL
  | BOOL
  | INT
  | STR
  | FN
  | BFN
  | ARRAY
  | HASH

type t =
  | ONull
  | OBoolean of bool
  | OInteger of int
  | OString of string
  | OFunction of fn
  | OBuiltin of (t list -> (t, string) result)
  | OArray of t list
  | OHash of (t, t) Hashtbl.t

and fn = {
  fn_params : Ast.identifier list;
  fn_body : Ast.block_statement;
  fn_env : t Environment.t;
}

let rec string_of_obj = function
  | ONull ->
      "null"
  | OBoolean b ->
      string_of_bool b
  | OInteger i ->
      string_of_int i
  | OString s ->
      s
  | OFunction fn ->
      let arity = List.length fn.fn_params |> string_of_int in
      "fn[" ^ arity ^ "]"
  | OBuiltin _ ->
      "fn_builtin"
  | OArray objects ->
      let inner = List.map string_of_obj objects |> String.concat ", " in
      "[" ^ inner ^ "]"
  | OHash _ ->
      "hash"

let type_of = function
  | ONull ->
      NULL
  | OBoolean _ ->
      BOOL
  | OInteger _ ->
      INT
  | OString _ ->
      STR
  | OFunction _ ->
      FN
  | OBuiltin _ ->
      BFN
  | OArray _ ->
      ARRAY
  | OHash _ ->
      HASH

let null_obj = ONull
let true_obj = OBoolean true
let false_obj = OBoolean false
let obj_of_bool b = if b then true_obj else false_obj

let is_truthy = function
  | OBoolean false
  | ONull ->
      false
  | _ ->
      true

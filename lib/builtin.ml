open Obj

let len = function
  | [ obj ] -> (
      match obj with
      | OString s ->
          Ok (OInteger (String.length s))
      | OArray objects ->
          Ok (OInteger (List.length objects))
      | _ ->
          Error "len: bad argument type")
  | _ ->
      Error "len: wrong number of arguments"

let first = function
  | [ obj ] -> (
      match obj with
      | OArray [] ->
          Ok null_obj
      | OArray (o :: _) ->
          Ok o
      | _ ->
          Error "first: bad argument type")
  | _ ->
      Error "first: wrong number of arguments"

let last = function
  | [ obj ] -> (
      match obj with
      | OArray objs ->
          let rec last = function
            | [] ->
                Ok null_obj
            | [ o ] ->
                Ok o
            | _ :: os ->
                last os
          in
          last objs
      | _ ->
          Error "last: bad argument type")
  | _ ->
      Error "last: wrong number of arguments"

let rest = function
  | [ obj ] -> (
      match obj with
      | OArray objs -> (
          match objs with
          | [] ->
              Ok null_obj
          | _ ->
              Ok (OArray (List.tl objs)))
      | _ ->
          Error "rest: bad argument type")
  | _ ->
      Error "rest: wrong number of arguments"

let push = function
  | [ arry; obj ] -> (
      match arry with
      | OArray objs ->
          Ok (OArray (objs @ [ obj ]))
      | _ ->
          Error "push: bad argument type")
  | _ ->
      Error "push: wrong number of arguments"

let puts objects =
  let put o = print_endline (string_of_obj o) in
  List.iter put objects;
  Ok null_obj

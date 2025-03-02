type key = string

type 'a t = {
  store : (key, 'a) Hashtbl.t;
  outer : 'a t option;
}

let rec get t n =
  match Hashtbl.find_opt t.store n with
  | Some v ->
      Some v
  | None -> (
      match t.outer with
      | Some outer ->
          get outer n
      | None ->
          None)

let set t n o = Hashtbl.replace t.store n o
let create ?outer () = { store = Hashtbl.create 22; outer }

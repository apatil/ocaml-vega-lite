(* Some support for monads, to lighten up the syntax *)
let bind x ~f = match x with
  | Ok x_ -> f x_
  | Error s -> Error s
let map x ~f = match x with
  | Error s -> Error s
  | Ok v -> Ok (f v)
let return x = Ok x
let both (x : ('a, 'b) result) (y : ('c, 'b) result) : (('a * 'c), 'b) result =
  match (x, y) with
  | (Ok v1, Ok v2) -> Ok (v1, v2)
  | (Error e, _) -> Error e
  | (_, Error e) -> Error e
let (>>=) x f = bind x f
let (>>|) x f = map x f

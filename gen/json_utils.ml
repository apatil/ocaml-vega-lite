open Parse_utils
open Let_syntax

(* Some utilities for working with json *)
type json = Yojson.Safe.json

let assertAssoc (j : json) : ((string * json) list, string) result =
  match j with
  | `Assoc l -> Ok l
  | _ -> Error "assertAssoc: Must be an assoc"

let assertList (j : json) : (json list, string) result =
  match j with
  | `List l -> Ok l
  | _ -> Error "assertList: Must be a list"

let assertString (j : json) : (string, string) result =
  match j with
  | `String s -> Ok s
  | _ -> Error "assertString: Must be a string"

let assertInt (j : json) : (int, string) result =
  match j with
  | `Int i -> Ok i
  | _ -> Error "assertInt: Must be an Int"

let assertStringList (j : json) : (string list, string) result =
  let reducer (sofar : (string list, string) result) (next : json) : (string list, string) result =
    let%bind
      sofarOk = sofar
    and s = assertString next
    in
    Ok (s :: sofarOk)
  in
  assertList j >>= fun (l) ->
  List.fold_left reducer (Ok []) l

let jsonKeys (j : json) : (string list, string) result =
  assertAssoc j >>= fun (l) -> Ok (List.map (fun (k,v) -> k) l)

let jsonGet (j : json) (key : string) : (json, string) result =
  assertAssoc j >>= fun (l) -> try
    let (k,v) = List.find (fun (k, v) -> String.equal key k) l in Ok v
  with Not_found -> Error ("jsonGet: Key '" ^ key ^ "' not found")

let jsonKeyIs (j : json) (k : string) (v : string) : (unit, string) result =
  (jsonGet j k) >>= assertString >>= (assertStringEqual v)

let assertAbsent (j : json) (k : string) : (unit, string) result =
  match jsonGet j k with
  | Ok _ -> Error ("assertAbsent: Key '" ^ k ^ "' is present")
  | _ -> Ok ()

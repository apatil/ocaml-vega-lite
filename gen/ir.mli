type typeName = string
type description = string option
type required = bool
type ctor = (string * typeRef list)
and field = (string * typeRef * description * required)
and typeRef =
  | Num of int option * int option (*min, max*)
  | String
  | Bool
  | Null
  | JSON (* Note, when 'object' occurs in type references it means 'any'. We represent this as Yojson *)
  | Ref of string
  | List of typeRef

type typeSpec =
  (* EG, | a | b of (t1 * t2). The string in the inner tuple is a description *)
  | Variant of ctor list
  | Record of field list
  | Alias of typeRef

type t = (typeName * typeSpec * description) list

val parse : Json_utils.json -> (t, string) result

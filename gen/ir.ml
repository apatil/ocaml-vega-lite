open Let_syntax
open Parse_utils
open Json_utils


(*
Utilities for converting JSON to the intermediate representation, which is the type
of this module.
*)

type description = string option
type required = bool
type typeName = string

(*
A variant constructor name, list of parameter types. In the case of enums, the list
of parameter types will be empty.
*)
type ctor = (string * typeRef list)

(* A record field name, type and description *)
and field = (string * typeRef * description * required)

(* The type of references to other types within type definitions *)
and typeRef =
  | Num of int option * int option
  | Float
  | Int
  | String
  | Bool
  | Null
  | JSON (* Note, when 'object' occurs in type references it means 'any'. We represent this as Yojson *)
  | AnonymousVariant of ctor list
  | Ref of string
  | List of typeRef
let rec equalTypeRef (tr1 : typeRef) (tr2 : typeRef) : bool =
  match (tr1, tr2) with
  | (Num ((Some mi1), (Some ma1)), Num ((Some mi2), (Some ma2))) -> (mi1 == mi2) && (ma1 == ma2)
  | (Num ((Some i1), None), Num ((Some i2), None)) -> i1 == i2
  | (Num (None, (Some i1)), Num (None, (Some i2))) -> i1 == i2
  | (Num (None, None), Num (None, None)) -> true
  | (String, String) -> true
  | (Bool, Bool) -> true
  | (Null, Null) -> true
  | (JSON, JSON) -> true
  | (AnonymousVariant cl1, AnonymousVariant cl2) -> let ctor_equal ((n1, tl1) , (n2, tl2)) =
      (String.equal n1 n2) && List.for_all (fun (ctr1, ctr2) -> equalTypeRef ctr1 ctr2) @@ List.combine tl1 tl2
    in
    List.for_all ctor_equal @@ List.combine cl1 cl2
  | (Ref s1, Ref s2) -> String.equal s1 s2
  | (List x1, List x2) -> equalTypeRef x1 x2
  | _ -> false

(*
The only types specified in the schemas are various kinds of variants & records.
Note, variants can sometimes specify other variants or records at non-top level.

Other types, like strings and arrays, are _referenced_ but not _defined_; so they're
in typeRef but not typeSpec
*)
let map_of_bindings (bindings: (string * 'a) list) : 'a StringMap.t =
  let reducer sofar (k, v) =
    StringMap.add k v sofar
  in
  List.fold_left reducer StringMap.empty bindings
let list_equal (compare : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list) : bool =
  match (List.length l1) == (List.length l2) with
  | false -> false
  | true -> let reducer (sofar : bool) ((n1, n2) : ('a * 'a)) : bool =
              match sofar with
              | false -> false
              | true -> compare n1 n2
    in
    List.fold_left reducer true (List.map2 (fun e1 e2 -> (e1, e2)) l1 l2)
type typeSpec =
  (* EG, | a | b of (t1 * t2). The string in the inner tuple is a description *)
  | Variant of ctor list
  | Record of (field list * typeRef option)
  | Alias of typeRef

let map_of_fieldlist (fl : field list) : typeRef StringMap.t =
  let binding_of_field ((name, tr, desc, _) : field) : (string * typeRef) =
    match desc with
    | None -> (name, tr)
    | Some d -> (name ^ "-S4d2HpceC7-" ^ d, tr)
  in
  List.map binding_of_field fl |> map_of_bindings

let equalTypeSpec (t1 : typeSpec) (t2 : typeSpec) : bool =
  match (t1, t2) with
  | (Variant cl1, Variant cl2) ->
    let s1 = map_of_bindings cl1 in
    let s2 = map_of_bindings cl2 in
    StringMap.equal (list_equal equalTypeRef) s1 s2
  | (Record (fl1, a1), Record (fl2, a2)) ->
    let s1 = map_of_fieldlist fl1 in
    let s2 = map_of_fieldlist fl2 in
    (StringMap.equal equalTypeRef s1 s2) && (match (a1, a2) with
        | (Some tr1, Some tr2) -> equalTypeRef tr1 tr2
        | (None, None) -> true
        | _ -> false)
  (* Aliases are always distinct, that's the point of them *)
  | (Alias s1, _) -> false
  | (_, Alias s2) -> false
  | _ -> false

let equalTSAndDesc ((t1, d1) : (typeSpec * description)) ((t2, d2) : (typeSpec * description)) : bool =
  let descsEqual = match (d1, d2) with
    | (None, None) -> true
    | (Some s1, Some s2) -> String.equal s1 s2
    | _ -> false
  in
  match descsEqual with
  | false -> false
  | true -> equalTypeSpec t1 t2

(* Convert any old name to a suitable module name. *)
let chooseModuleName (name : string) : string =
  String.capitalize_ascii name

(* The IR data structure is a map from type names to type specifications. *)
type accum = (typeSpec * description) StringMap.t

(* A merge function for two sets of type specs that makes sure no names are duplicated. *)
let merge (t1 : accum) (t2 : accum) : (accum, string) result =
  let reducer sofar (k, v) =
    let%bind mSofar = sofar in
    match mapGet k mSofar with
    | Ok v2 -> (match equalTSAndDesc v v2 with
        | true -> Ok mSofar
        | false -> Error ("merge: Duplicate key with distinct values: '" ^ k ^ "'"))
    | _ -> Ok (StringMap.add k v mSofar)
  in
  List.fold_left reducer (Ok t1) (StringMap.bindings t2)


(*
Gets a description string for a type spec or record field. Not all of these have
descriptions in the schema, so this returns an option.
*)
let getDescription (node : json) : description =
  match jsonGet node "description" >>= assertString with
  | Ok d -> Some d
  | _ -> None

(* Choose a variant name from a string. *)
let chooseVariantName (raw : string) : string =
  String.capitalize_ascii @@ Str.global_replace (Str.regexp "-") "_" raw

(*
In the schema, anyOf's don't need ML-like variant constructors to wrap the different
type options; they're straight up dynamically typed. In OCaml, we do need to wrap
the different type variants in constructors. Since these ctors aren't mentioned in the
schema, we have to make up our own names for them.
*)
let rec chooseCtorName (name : string) (typ : typeRef) : string =
  match typ with
  | Ref s -> chooseVariantName s
  | Num _ -> "Num"
  | Float -> "Float"
  | Int -> "Int"
  | String -> "String"
  | Bool -> "Bool"
  | Null -> "Null"
  | List t -> (chooseCtorName name t) ^ "s"
  | AnonymousVariant _ -> "Variant"
  | JSON -> "Json"

let makeUnique (sofar : accum) (name : string) : (string, string) result =
  let suffixes = [""; "_1"; "_2"; "_3"; "_4"; "_5"] in
  let reducer (rsofar : (string, string) result) (next : string) : (string, string) result =
    match rsofar with
    | Ok s -> Ok s
    | Error _ -> (let nextTry = name ^ next in
      match StringMap.exists (fun s _ -> (String.equal s nextTry)) sofar with
      | true -> Error ("Name already taken: '" ^ nextTry ^ "'")
      | false -> Ok nextTry)
  in
  List.fold_left reducer (Error "Init") suffixes

let chooseTypeName (name : string) (sofar : accum) (ts : typeSpec) : (string, string) result =
  match ts with
  | Variant vl -> makeUnique sofar (name ^ "_Variant")
  | Record _ -> makeUnique sofar name
  | Alias _ -> Error "chooseTypeName: can't handle alias, why would you even want that."


(*
The type for parsers. Each takes a JSON node and tries to compile it to a set of type
specs. If the parse is successful, the result will include the type spec for the node
itself as well as any anonymous type specs contained within that type spec.
*)
type 'a parser = accum -> string -> json -> ('a, string) result

let wrapWithInnerTypes (trp : typeSpec parser) : ((typeSpec * accum) parser) = fun sofar name node ->
  let%bind tr = trp sofar name node in
  Ok (tr, StringMap.empty)

(*
Parses a variant whose constructors are all singletons, ie an enum.
Cannot create inner types.
*)
let parseFlatVariant : typeSpec parser = fun sofar name node ->
  prependError "parseFlatVariant: " @@
  (*
    If this node is an assoc with keys called "enums" and "type", and "type" is "string",
    then it is an enum/flat variant and we can proceed. Otherwise, we have to bail and
    hope another parser can manage the node.
  *)
  let%bind
    variantNames = jsonGet node "enum" >>= assertStringList
    and typ = jsonGet node "type" >>= assertString
  in
  match typ with
  | "string" -> (
      (*
        Assuming the above conditions are met, the parse will succeed. We just need to
        augment the constructors with empty parameter lists, then wrap them up in a typeSpec.
      *)
      let ctors = List.map (fun n -> ((chooseVariantName n), [])) variantNames in
      Ok (Variant ctors)
    )
  | _ -> Error "Type is not a string"


let parseNumber : typeRef parser = fun sofar name node ->
  prependError "parseNumber: " @@
  jsonKeyIs node "type" "number" >>= fun () ->
  let%bind minv = match jsonGet node "minimum" with
    | Ok m -> assertInt m >>= fun mv -> Ok (Some mv)
    | _ -> Ok None
  in
  let%bind maxv = match jsonGet node "maximum" with
    | Ok m -> assertInt m >>= fun mv -> Ok (Some mv)
    | _ -> Ok None
  in
  Ok (Num (minv, maxv))

let parseEmptyNode : typeRef parser = fun sofar name node ->
  match node with
  | `Assoc [] -> Ok JSON
  | _ -> Error "parseEmptyNode: Node is not empty"

let parseSimpleType : typeRef parser = fun sofar name node ->
  prependError "parseSimpleType: " @@
  let%bind s = jsonGet node "type" >>= assertString in
  match s with
    | "string" -> Ok String
    | "boolean" -> Ok Bool
    | "null" -> Ok Null
    | "number" -> parseNumber sofar name node
    | _ -> Error "No parse"

let getRefName (node : json) : (string, string) result =
  prependError "getRefName: " @@
  let%bind rawRef = jsonGet node "$ref" >>= assertString in
  let re = Str.regexp "#/definitions/\\(.*\\)" in
  (* The regexp api is not great *)
  try
    let _ = Str.string_match re rawRef 0 in
    Ok (Str.matched_group 1 rawRef)
  with _ -> Error "Unrecognized ref string format"

(* Parses references, eg "$ref": "#/definitions/blarg" *)
let parseReference : typeRef parser = fun sofar name node ->
  prependError "parseReference: " @@
  let%bind refString = getRefName node in
  let refName = (chooseModuleName refString) in
  Ok (Ref refName)

(*
Parses variants specified with lists of simple types, eg ["string" "bool"]
Cannot create inner types
*)
let rec parseSimpleVariant : typeSpec parser = fun sofar name node ->
  prependError "parseSimpleVariant: " @@
  (* First, check that it's a list of strings. *)
  let%bind els = jsonGet node "type" >>= assertStringList in
  let reducer (sofar : (ctor list, string) result) (next : string) : (ctor list, string) result =
    let%bind ctorsSofar = sofar in
    match next with
    | "string" -> Ok (("String", [String]) :: ctorsSofar)
    | "number" -> Ok (("Num", [Num (None, None)]) :: ctorsSofar)
    | "boolean" -> Ok (("Bool", [Bool]) :: ctorsSofar)
    | "null" -> Ok (("Null", [Null]) :: ctorsSofar)
    | _ -> Error "Unrecognized simple type"
  in
  let%bind ctors = List.fold_left reducer (Ok []) els in
  Ok (Variant ctors)


let rec parseVariant : (typeSpec * accum) parser = fun tSofar name node ->
  prependError "parseVariant: " @@
  (*
  We first check that the node is an assoc containing the anyOf key and that its value
  is a List node. If these checks fail, we bail and hope that another parser can handle
  the node.

  There are checks on each element of the anyOf inside the reducer below. If any of
  these checks fail, the full parse fails.
  *)
  let%bind variantNodes = jsonGet node "anyOf" >>= assertList in
  (*
  The reducer accumulates a set of types containing the internal type specs, and
  also a list of the variant's constructor specs.
  *)
  let reducer (sofar : ((accum * (ctor list)), string) result) (next : json) : ((accum * (ctor list)), string) result =
    (* First, check that the parse has not already failed. *)
    let%bind (innerTypesSofar, ctorsSofar) = sofar in

    (* If the constructor is a variant, flatten it into the full variant. *)
    let variantCase : ((accum * (ctor list)), string) result =
      let%bind (v, innerTypes) = untilSuccess "variantCase: " [
        (wrapWithInnerTypes parseFlatVariant) tSofar "__name" next;
        (wrapWithInnerTypes parseSimpleVariant) tSofar "__name" next;
        parseVariant tSofar "__name" next
      ]
      in
      let%bind allInnerTypes = merge innerTypesSofar innerTypes in
      match v with
      | Variant ctors -> Ok (allInnerTypes, List.append ctorsSofar ctors)
      | _ -> Error "Unexpected non-variant type from parseVariant"
    in
    (* Otherwise, parse it as a parameter-bearing constructor. *)
    let otherCase : ((accum * (ctor list)), string) result =
      (*
        Check that the node under consideration represents a valid type reference. In the
        process of parsing the type reference we may have to emit anonymous type specs,
        so the full set of types so far may be augmented before we even generate a type
        for the variant.
      *)
      let%bind takenNames = merge innerTypesSofar tSofar in
      let%bind (ctorTypeRef, innerTypes) = (parseTypeRef takenNames name next) in
      (*
        Check that we haven't duplicated the name of an existing type. If we haven't,
        update the running set of inner types.
      *)
      let%bind allInnerTypes = (merge innerTypesSofar innerTypes) in
      (*
        Finally, append the constructor represented by the current node to the list of
        cosntructors.
      *)
      let ctorName = chooseCtorName name ctorTypeRef in
      let allCtors : ctor list = (ctorName, [ctorTypeRef]) :: ctorsSofar in
      Ok (allInnerTypes, allCtors)
    in
    untilSuccess "No parse" [variantCase; otherCase]
  in
  let%bind (innerTypes, ctors) = List.fold_left reducer (Ok (StringMap.empty, [])) variantNodes in
  (*
  Once all the constructors are parsed, the parse can't fail. We just need to bundle
  up the full variant type from the constructors, aand return it with the set of inner
  types identified during the parse..
  *)
  Ok ((Variant ctors), innerTypes)

and parseRecord : (typeSpec * accum) parser = fun tSofar name node ->
  prependError "parseRecord: " @@
  (*
  For this parser to handle the node, type must be 'object' and either 'properties'
  or 'additionalProperties' must be defined
  *)
  let%bind _ = jsonGet node "type" >>= assertString >>= assertStringEqual "object" in
  (*
    First, see whether the three main fields properties, additionalProperties
    and required are present and, if present, make sure they are legal.

    Properties defines the fields owned by this record. It's not always present,
    sometimes there is only additionalProperties. If neither properties nor
    additionalProperties is present, it's json and shouldn't be handled by this
    parser.

    Required sometimes lists the field names that are required. I guess that means
    anything not listed can be left out of the json.

    additionalProperties sometimes holds a ref to another object/record. In those
    cases, the fields from that object/record should be inlined into this one in
    the json. However, we'll keep them in a sub-record in the type system to make
    them easier to reuse.
  *)
  let%bind properties = match jsonGet node "properties" with
    | Error _ -> Ok []
    | Ok p -> assertAssoc p
  in
  let%bind additional = match jsonGet node "additionalProperties" with
    | Ok (`Bool false) -> Ok None
    | Ok a ->
      let%bind (tr, _) = parseTypeRef tSofar name a in
      Ok (Some tr)
    | Error _ -> Ok None
  in
  let%bind required = match jsonGet node "required" with
    | Ok n -> assertStringList n
    | _ -> Ok []
  in
  let%bind _ = match (properties, additional) with
    | ([], None) -> Error "Expected either 'properties' or 'additionalProperties'"
    | _ -> Ok ()
  in
  (* Now parse the 'properties' if present. *)
  let%bind (ownInnerTypes, ownFields) = begin
    match properties with
    | [] -> Ok (StringMap.empty, [])
    | props -> begin
      (*
      The reducer accumulates a set of types containing the internal type specs, and
      also a list of the variant's constructor specs.
      *)
      let reducer (sofar : ((accum * (field list)), string) result) ((fieldName, nextNode) : (string * json)) : ((accum * (field list)), string) result =
        (* First, check that the parse has not already failed. *)
        let%bind (innerTypesSofar, fieldsSofar) = sofar in
        prependError ("Field '" ^ fieldName ^ "': ") @@
        (*
          Check that the node under consideration represents a valid type reference. In the
          process of parsing the type reference we may have to emit anonymous type specs,
          so the full set of types so far may be augmented before we even generate a type
          for the record.
        *)
        let%bind (fieldTypeRef, innerTypes) = (parseTypeRef tSofar (name ^ "_" ^ (chooseModuleName fieldName)) nextNode) in
        (*
          Check that we haven't duplicated the name of an existing type. If we haven't,
          update the running set of inner types.
        *)
        let%bind allInnerTypes = (merge innerTypesSofar innerTypes) in
        (* See if the field has a description. *)
        let desc = getDescription nextNode in
        (* See if the field is required. *)
        let isRequired = List.exists (String.equal fieldName) required in
        (*
          Finally, append the field represented by the current node to the list of
          fields.
        *)
        let allFields : field list = (fieldName, fieldTypeRef, desc, isRequired) :: fieldsSofar in
        Ok (allInnerTypes, allFields)
      in
      List.fold_left reducer (Ok (StringMap.empty, [])) props
    end
  end
  in
  (* Combine properties with additionalProperties and return. *)
  Ok ((Record (ownFields, additional)), ownInnerTypes)

and parseTR_anonVariant : (typeRef * accum) parser = fun sofar name node ->
  let%bind (typ, innerTypes) = untilSuccess "parseTR_anonVariant: No parse" [
    (wrapWithInnerTypes parseFlatVariant) sofar name node;
    (wrapWithInnerTypes parseSimpleVariant) sofar name node;
    parseVariant sofar name node;
  ]
  in
  match typ with
  | Variant ctors -> Ok (AnonymousVariant ctors, innerTypes)
  | _ -> Error "Expected to parse Variant or nothing"

(*
  Parse an anonymous record. We have to parse the record, then splice it into
  the inner type specs and return a reference to it.
*)
and parseTR_anonRecord : (typeRef * accum) parser = fun sofar name node ->
  prependError "parseTR_anonRecord: " @@
  let%bind (record, innerTypes) = parseRecord sofar name node in
  let%bind takenNames = merge innerTypes sofar in
  let%bind typName = chooseTypeName name takenNames record in
  let ref = Ref typName in
  Ok (ref, StringMap.add typName (record, None) innerTypes)

and parseTR_list : (typeRef * accum) parser = fun sofar name node ->
  prependError "parseTR_list: " @@
  jsonKeyIs node "type" "array" >>= fun () ->
  let%bind (itemTypeRef, innerTypes) = (jsonGet node "items") >>= (parseTypeRef sofar name) in
  Ok (List itemTypeRef, innerTypes)

and parseTR_reference : (typeRef * accum) parser = fun sofar name node ->
  prependError "parseTR_reference: " @@
  let%bind refr = parseReference sofar name node in
  Ok (refr, StringMap.empty)

and parseTR_json : (typeRef * accum) parser = fun sofar name node ->
  prependError "parseTR_simple: " @@
  match node with
  (* Sometimes an 'any' type reference is denoted "{}" *)
  | `Assoc [] -> Ok (JSON, StringMap.empty)
  (* Sometimes it is denoted {"type": "object"}, but we should let the record parser have first crack. *)
  | _ -> jsonKeyIs node "type" "object" >>= fun () -> Ok (JSON, StringMap.empty)

and parseTR_simple (errorName : string) (typeString: string) (ctorVal : typeRef) : (typeRef * accum) parser = fun sofar name node ->
  prependError (errorName ^ ": ") @@
  jsonKeyIs node "type" typeString >>= fun () ->
  Ok (ctorVal, StringMap.empty)

and parseTR_number : (typeRef * accum) parser = fun sofar name node ->
  prependError "parseTR_number: " @@
  let%bind num = parseNumber sofar name node in
  Ok (num, StringMap.empty)

(*
Parses a type reference. Usually the 't' result will be an empty map; but if
the type reference creates inner anonymous types, that won't be the case.
*)
and parseTypeRef : (typeRef * accum) parser = fun sofar name node ->
  let parseTR_string = parseTR_simple "parseTR_string" "string" String in
  let parseTR_boolean = parseTR_simple "parseTR_boolean" "boolean" Bool in
  let parseTR_null = parseTR_simple "parseTR_null" "null" Null in
  untilSuccess "parseTypeRef: no parse" [
    parseTR_list sofar name node;
    parseTR_number sofar name node;
    parseTR_string sofar name node;
    parseTR_boolean sofar name node;
    parseTR_null sofar name node;
    parseTR_reference sofar name node;
    parseTR_anonRecord sofar name node;
    parseTR_anonVariant sofar name node;
    parseTR_json sofar name node;
  ]

and parseAlias : typeSpec parser = fun sofar name node ->
  let modName = chooseModuleName name in
  let%bind tr = untilSuccess ("parseAliasSpec: no parse of '" ^ name ^ "'") [
      parseReference sofar modName node;
      parseSimpleType sofar modName node;
      parseEmptyNode sofar modName node;
    ] in
  Ok (Alias tr)

(*
Parses a variant that isn't an enum, ie an allOf. The constructors are not mentioned
explicitly in the spec, we need to introduce them in ocaml for polymorphism. Most will
take parameters.
*)
let parseVariantSpec : accum parser = fun sofar name node ->
  let modName = chooseModuleName name in
  let%bind (myType, innerTypes) = untilSuccess "parseVariantSpec: " [
    (wrapWithInnerTypes parseFlatVariant) sofar modName node;
    (wrapWithInnerTypes parseSimpleVariant) sofar modName node;
    parseVariant sofar modName node
  ] in
  Ok (StringMap.add modName (myType, (getDescription node)) innerTypes)

(* Parses a record type spec, represented in the schema as an object. *)
let parseRecordSpec : accum parser = fun sofar name node ->
  let modName = chooseModuleName name in
  let%bind (myType, innerTypes) = parseRecord sofar modName node in
  Ok (StringMap.add modName (myType, (getDescription node)) innerTypes)

(* Parses a type alias, represented in the schema as a type reference. *)
let parseAliasSpec : accum parser = fun sofar name node ->
  let modName = chooseModuleName name in
  let%bind myType = parseAlias sofar name node in
  Ok (StringMap.singleton modName (myType, None))

(*
Parses a top-level type spec. These are all variants or records, there are no
type aliases for simpler types defined.
*)
let parseTypeSpec : accum parser = fun sofar name node ->
  untilSuccess ("parseTypeSpec: no parse of '" ^ name ^ "'") [
    parseVariantSpec sofar name node; (* NOTE: Must go before 'alias' to prevent it from snaffling enums *)
    parseAliasSpec sofar name node;
    parseRecordSpec sofar name node
  ]

type t = (typeName * typeSpec * description) list

let rec get_tr_references (tr : typeRef) : string list =
  match tr with
  | List el -> get_tr_references el
  | Ref s -> [s]
  | _ -> []

let rec get_references (t : typeSpec) : string list =
  match t with
  | Alias tr -> get_tr_references tr
  | Variant ctors -> List.map (fun (_, trs) -> List.map get_tr_references trs |> List.flatten) ctors |> List.flatten
  | Record (fields, None) -> List.map (fun (_, tr, _, _) -> get_tr_references tr) fields |> List.flatten
  | Record (fields, Some a) -> List.flatten [(get_tr_references a); get_references @@ Record (fields, None)]

(* let graph_of_accum (a : accum) : TopoSort.graph =
  let reducer (sofar : TopoSort.graph) ((name, (spec, desc)) : (string * (typeSpec * description))) : TopoSort.graph =
    let refs = get_references spec in
    let _ = if (List.length refs) > 0 then print_endline ("References in " ^ name ^ " are " ^ (String.concat ", " refs)) else () in
    (name, get_references spec) :: sofar
  in
  List.fold_left reducer [] (StringMap.bindings a)

let tsort (a : accum) : (t, string) result =
  let g = graph_of_accum a in
  let%bind sorted_names = TopoSort.toposort g in
  let reducer (mSofar : (t, string) result) (next : string) : (t, string) result =
    let%bind sofar = mSofar in
    let%bind (spec, desc) = mapGet next a in
    Ok ((next, spec, desc) :: sofar)
  in
  List.fold_left reducer (Ok []) sorted_names *)


(* The top-level parser *)
let parse (node : json) : (t, string) result =
  prependError "parse: " @@
  let%bind defs = jsonGet node "definitions" >>= assertAssoc in
  let reducer (sofar : (accum, string) result) ((name, node): (string * json)) : (accum, string) result =
    let%bind sofarOk = sofar in
    let%bind newTypes = parseTypeSpec sofarOk name node in
    prependError ("name '" ^ name ^ "': ") @@
    merge sofarOk newTypes
  in
  let%bind a = List.fold_left reducer (Ok StringMap.empty) defs in
  (*
  TODO: The topological sort currently can't handle cycles, and there are cycles, so
  just have to make everything mutually recursive. To make this more fine grained in
  future.
  *)
  (* tsort a *)
  StringMap.bindings a |> List.map (fun (name, (spec, desc)) -> (name, spec, desc)) |> fun x -> Ok x

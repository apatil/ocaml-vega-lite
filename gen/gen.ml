module Gu = Gen_utils

exception NullType

let load_file (f : string) : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let save_file (f : string) (contents : string) : unit =
  let channel = open_out f in
  output_string channel contents;
  close_out channel

let mangle_unsafe_identifiers (raw : string) : string =
  match raw with
  | "type" -> "type_"
  | "and" -> "and_"
  | "as" -> "as_"
  | "or" -> "or_"
  | "$schema" -> "schema"
  | _ -> raw

let additionalFieldName = "additional"

let ctorWrapsNull = function
  | (_, [Ir.Null]) -> true
  | _ -> false
let ctorNotWrapsNull c = match ctorWrapsNull c with
  | true -> false
  | false -> true

let optionalizeType (typ : Parsetree.core_type) : Parsetree.core_type =
  match typ with
  | [%type: [%t? typ_] option] -> typ
  | _ -> [%type: [%t typ] option]

let rec type_of_tr (prefix : string) (tr : Ir.typeRef) : Parsetree.core_type =
  let open Ir in
  match tr with
  | Num (min, max) -> [%type: [`Int of int | `Float of float]]
  | Float -> [%type: float]
  | Int -> [%type: int]
  | String -> [%type: string]
  | Bool -> [%type: bool]
  | JSON -> [%type: Yojson.Safe.json] (* Note, when 'object' occurs in type references it means 'any'. We represent this as Yojson *)
  | AnonymousVariant cl -> type_of_ctors prefix cl
  | Ref s -> Gu.simple_type (prefix ^ s ^ ".t")
  | List el -> [%type: [%t type_of_tr prefix el] list]
  | Null -> raise NullType

and rf_of_ctor (prefix : string) ((name, trs) : Ir.ctor) : Parsetree.row_field =
  let typs = List.map (type_of_tr prefix) trs in
  Parsetree.Rtag (name, [], false, typs)

and type_of_ctors (prefix : string) (ctors: Ir.ctor list) : Parsetree.core_type =
  let isOption = List.exists ctorWrapsNull ctors in
  let rfs = List.map (rf_of_ctor prefix) (List.filter ctorNotWrapsNull ctors) in
  let typ = Gu.type_of_desc @@ Parsetree.Ptyp_variant (rfs, Asttypes.Closed, None) in
  match isOption with
  | true -> optionalizeType typ
  | false -> typ

let ld_of_field ((name, tr, _, required) : Ir.field) : Parsetree.label_declaration =
  let pname = mangle_unsafe_identifiers name in
  let typ = match required with
    | true -> type_of_tr "" tr
    | false -> optionalizeType @@ type_of_tr "" tr
  in
  Gu.label_decl pname typ

let stringmap_type_of_tr (tr : Ir.typeRef) : Parsetree.core_type =
  [%type: (string * [%t type_of_tr "" tr]) list]

let tdecl_of_fields (derivers : string list) (fields: Ir.field list) (additional : Ir.typeRef option) : Parsetree.type_declaration =
  match (additional, fields) with
  | (None, _) -> Gu.rectype_of_labels derivers "t" @@ List.map ld_of_field fields
  | (Some tr, []) -> Gu.type_decl_of_type derivers "t" @@ stringmap_type_of_tr tr
  | (Some tr, _) -> Gu.rectype_of_labels derivers "t" @@ (Gu.label_decl additionalFieldName @@ stringmap_type_of_tr tr) :: List.map ld_of_field fields

let rec si_of_ts (derivers : string list) (spec: Ir.typeSpec) : Parsetree.structure_item =
  match spec with
  | Ir.Alias tr -> [%stri type t = [%t type_of_tr "" tr]]
    (*
    Note: if you want these to be non-backticked variants, you need to build a
    type_declaration wrapping a Ptype_variant.
    *)
  | Ir.Variant ctors -> [%stri type t = [%t type_of_ctors "" ctors]]
    (*
    If this is a singleton record, treat it as an alias. We can't completely
    squash it in the IR because we need the to_json function to preserve the
    field name.
    *)
  | Ir.Record ([(fname, tr, _, _)], None) -> si_of_ts derivers (Ir.Alias tr)
  | Ir.Record (fields, additional) -> Gu.sitem_of_tdecl @@ tdecl_of_fields derivers fields additional

let rec sigi_of_ts (derivers : string list) (spec: Ir.typeSpec) : Parsetree.signature_item =
  match spec with
  | Ir.Alias tr -> [%sigi: type t = [%t type_of_tr "" tr]]
    (*
    Note: if you want these to be non-backticked variants, you need to build a
    type_declaration wrapping a Ptype_variant.
    *)
  | Ir.Variant ctors -> [%sigi: type t = [%t type_of_ctors "" ctors]]
    (*
    If this is a singleton record, treat it as an alias. We can't completely
    squash it in the IR because we need the to_json function to preserve the
    field name.
    *)
  | Ir.Record ([(fname, tr, _, _)], _) -> sigi_of_ts derivers (Ir.Alias tr)
  | Ir.Record (fields, additional) -> Gu.sigitem_of_tdecl @@ tdecl_of_fields derivers fields additional

(*
Indents all lines in a string to the given number of spaces.
*)
let indent (places : int) (raw : string) : string =
  let lines = Str.split (Str.regexp "\n") raw in
  let spacer = "\n" ^ (String.concat "" (Array.to_list (Array.make places " "))) in
  spacer ^ (String.concat spacer lines)

(*
Takes the top-level description of an IR type and, in the case of records, the
field descriptions and compiles a doc comment.
*)
let doc_comment_of_desc (spec : Ir.typeSpec) (desc : Ir.description) : string =
  let descStr = match desc with
    | Some s -> s
    | None -> ""
  in
  let fullDesc = match spec with
    | Ir.Record (fields, _) -> (
        let reducer (sofar : string list) ((fname, _, fdesc, _) : Ir.field) : string list =
          match fdesc with
          | Some s -> let fstr = ("  " ^ fname ^ ":" ^ (indent 4 s)) in
            (match sofar with
             | [] -> let hdr = (match desc with
                 | Some s -> "\n\n\nFields:"
                 | None -> "Fields")
               in
              [fstr; hdr]
             | x -> fstr :: x)
          | None -> sofar
        in
        descStr ^ String.concat "\n\n" @@ List.rev (List.fold_left reducer [] fields)
      )
    | _ -> descStr
  in
  match fullDesc with
  | "" -> ""
  | x -> indent 2 @@ "(** " ^ fullDesc ^ "\n*)"

let rec to_json_of_tr (tr : Ir.typeRef) (alreadyOptional : bool) : Parsetree.expression =
  let open Ir in
  match tr with
  | Num (min, max) -> [%expr function
    | `Int i -> `Int i
    | `Float f -> `Float f]
  | Float -> [%expr fun (f : float) -> (`Float f : Yojson.Safe.json)]
  | Int -> [%expr fun (i : int) -> (`Int i : Yojson.Safe.json)]
  | String -> [%expr fun (s : string) -> (`String s : Yojson.Safe.json)]
  | Bool -> [%expr fun (b : bool) -> (`Bool b : Yojson.Safe.json)]
  | JSON -> [%expr fun (j : Yojson.Safe.json) -> (j : Yojson.Safe.json)]
  | AnonymousVariant cl -> to_json_of_ctors cl alreadyOptional
  | Ref s -> [%expr fun (x : [%t Gu.simple_type (s ^ ".t")]) -> (([%e Gu.ident_expr (s ^ ".to_yojson")] x) : Yojson.Safe.json)]
  | List el -> [%expr fun (l : [%t type_of_tr "" el] list) ->
    let yojson_of_el= [%e to_json_of_tr el false]  in
    (`List (List.map yojson_of_el l) : Yojson.Safe.json)
  ]
  | Null -> raise NullType

and to_json_of_ctors (ctors : Ir.ctor list) (alreadyOptional : bool) : Parsetree.expression =
  let isOption = not alreadyOptional && List.exists ctorWrapsNull ctors in
  let case_of_opt_ctor ((name, trs) : Ir.ctor) : Parsetree.case option =
    let sname = String.uncapitalize_ascii name in
    Some (match trs with
    (* In the case of a flat variant, it needs to be translated to a string. *)
    | [] -> Gu.case_of_pat_and_expr (Gu.variant name |> Gu.construct "Some") [%expr `String [%e Gu.expr_of_string sname]]
    | fst :: snd :: _ -> let () = print_endline name in raise NullType
    | Ir.Null :: [] -> Gu.case_of_pat_and_expr (Gu.pat_of_name "None") [%expr `Null]
    | tr :: [] ->
      Gu.case_of_pat_and_expr (Gu.pat_of_name "v" |> Gu.variant_with_arg name |> Gu.construct "Some") [%expr [%e to_json_of_tr tr false] [%e Gu.ident_expr "v"]])
  in
  let case_of_req_ctor ((name, trs) : Ir.ctor) : Parsetree.case option =
    let sname = String.uncapitalize_ascii name in
    match trs with
    (* In the case of a flat variant, it needs to be translated to a string. *)
    | [] -> Some (Gu.case_of_pat_and_expr (Gu.variant name) [%expr `String [%e Gu.expr_of_string sname]])
    | fst :: snd :: _ -> let () = print_endline name in raise NullType
    (* If we're looking at an anonymous variant for an optional field, just ignore nulls. *)
    | Ir.Null :: [] -> if alreadyOptional then
        None
      else
        raise NullType
    | tr :: [] ->
      Some (Gu.case_of_pat_and_expr (Gu.pat_of_name "v" |> Gu.variant_with_arg name) [%expr [%e to_json_of_tr tr false] [%e Gu.ident_expr "v"]])
  in
  let case_of_ctor (ctor : Ir.ctor) : Parsetree.case option =
    match isOption with
    | true -> case_of_opt_ctor ctor
    | false -> case_of_req_ctor ctor
  in
  Gu.expr_of_desc @@ Parsetree.Pexp_function (Gu.keep_somes @@ List.map case_of_ctor ctors)

let to_json_of_fields (additional: Ir.typeRef option) (fields : Ir.field list) : Parsetree.expression =
  let el_of_field ((name, tr, _, required) : Ir.field) : Parsetree.expression =
    let pname = mangle_unsafe_identifiers name in
    let valExpr = Gu.ident_expr ("x." ^ pname) in
    let jsonVal = match required with
      | true -> [%expr [%e to_json_of_tr tr false] [%e valExpr]]
      | false -> [%expr (function
          | None -> `Null
          | Some v -> [%e to_json_of_tr tr true] v) [%e valExpr]]
    in
    (* Use the unmangled, unsafe name here in the json. *)
    [%expr ([%e Gu.expr_of_string name], [%e jsonVal])]
  in
  let reducer (sofar : Parsetree.expression) (next : Ir.field) : Parsetree.expression =
    [%expr [%e el_of_field next] :: [%e sofar]]
  in
  let contents = List.fold_left reducer [%expr []] fields in
  [%expr fun x -> `Assoc (List.filter (fun (k, v) -> match v with | `Null -> false | _ -> true) [%e contents])]

let to_json_of_singleton_record (fname : string) (tr : Ir.typeRef) : Parsetree.expression =
  let rhs = [%expr [%e to_json_of_tr tr true] x] in
  [%expr fun x -> `Assoc [([%e Gu.expr_of_string fname], [%e rhs])]]

let to_json_of_additional_only (tr : Ir.typeRef) : Parsetree.expression =
  [%expr fun x ->
    let mapper (k, v) =
      (k, [%e to_json_of_tr tr true] v)
    in
    `Assoc (List.map mapper x)]

let to_json_of_ts (ts : Ir.typeSpec) : Parsetree.expression =
  match ts with
  | Ir.Alias tr -> to_json_of_tr tr false
  | Ir.Record ([(fname, tr, _, _)], None) -> to_json_of_singleton_record fname tr
  | Ir.Record([], Some tr) -> to_json_of_additional_only tr
  | Ir.Record (fields, additional) -> to_json_of_fields additional fields
  | Ir.Variant ctors -> to_json_of_ctors ctors false

(* let rec of_json_of_tr (tr : Ir.typeRef) : Parsetree.expression =
  let open Ir in
  match tr with
  | Num (min, max) -> [%expr function
    | `Int (i : int) -> Ok i
    | _ -> `Error "Expected number"
  ]
  | String -> [%expr function
    | `String (s : string) -> Ok s
    | _ -> Error "Expected string"
  ]
  | Bool -> [%expr function
    | `Bool (b : bool) -> Ok b
    | _ -> Error "Expected bool"
  ]
  | JSON -> [%expr fun (j : Yojson.Safe.json) -> Ok j]
  | AnonymousVariant _ -> [%expr fun _ -> Error "Not implemented"]
  | Ref s -> [%expr fun (j : Yojson.Safe.json) -> [%e Gu.ident_expr (s ^ ".of_yojson")] j]
  | List el -> [%expr function
    | `List a -> (
        let el_of_yojson el = [%e of_json_of_tr el] in
        flatten_results @@ List.map el_of_yojson a
    )
    | _ -> Error "Expected array"
  ]
  | Null -> raise NullType

let of_json_of_fields (fields : Ir.field list) : Parsetree.expression = [%expr fun _ -> Error "Not implemented"]

let of_json_of_ctors (ctors : Ir.ctor list) : Parsetree.expression = [%expr fun _ -> Error "Not implemented"]

let of_json_of_ts (ts : Ir.typeSpec) : Parsetree.expression =
  match ts with
  | Ir.Alias tr -> of_json_of_tr tr
  | Ir.Record fields -> of_json_of_fields fields
  | Ir.Variant ctors -> of_json_of_ctors ctors *)


(* TODO: DRY, move into gen_utils *)
let record_literal (fields: Ir.field list) (additional : Ir.typeRef option) : Parsetree.expression =
  let mapper ((fieldName, _, _, _) : Ir.field) : (string * Parsetree.expression) =
    let sfn = mangle_unsafe_identifiers fieldName in
    (sfn, Gu.ident_expr sfn)
  in
  let els = List.map mapper fields in
  Gu.record_literal None @@ match additional with
  | None -> els
  | Some tr -> (additionalFieldName, Gu.ident_expr additionalFieldName) :: els

let record_literal_with_none (fields : Ir.field list) : Parsetree.expression =
  let mapper ((fieldName, _, _, _) : Ir.field) : (string * Parsetree.expression) =
    let sfn = mangle_unsafe_identifiers fieldName in
    (sfn, [%expr None])
  in
  Gu.record_literal None @@ List.map mapper fields

let empty_record_of_ref (ir : Ir.t) (rn : string) : Parsetree.expression option =
  try
    let (_, spec, _) = List.find (fun (n, _, _) -> String.equal n rn) ir in
    match spec with
    | Ir.Record (flds, _) -> (match List.for_all (fun (_, _, _, r) -> not r) flds with
        | true -> Some (record_literal_with_none flds)
        | false -> None
      )
    | _ -> None
  with _ -> None

let dflt_of_field (ir : Ir.t) ((name, tr, _, required) : Ir.field) : Parsetree.expression option =
  match (name, tr, required) with
    | ("$schema", _, true) -> Some [%expr "https://vega.github.io/schema/vega-lite/v2.json"]
    | ("$schema", _, false) -> Some [%expr Some "https://vega.github.io/schema/vega-lite/v2.json"]
    | (_, _, false) -> Some [%expr None]
    | (_, (Ir.Ref rn),  true) -> (match empty_record_of_ref ir rn with
        | Some reclit -> Some (Gu.wrap_in_open rn reclit)
        | None -> None
      )
    | (_, _, true) -> None

let maker_of_fields (ir : Ir.t) (mname : string) (fields: Ir.field list) (additional: Ir.typeRef option) : (Parsetree.core_type * Parsetree.expression) =
  let fcomp fld1 fld2 =
    match (dflt_of_field ir fld1, dflt_of_field ir fld2) with
    | (Some _, None) -> 1
    | (None, Some _) -> -1
    | _ -> let ((n1, _, _, _), (n2, _, _, _)) = (fld1, fld2) in String.compare n1 n2
  in
  let some_required = List.exists (fun (_, _, _, r) -> r) fields in
  let fields = List.sort fcomp fields in
  let reclit = record_literal fields additional in
  let rect = Gu.simple_type "t" in
  let res = match some_required with
    | true -> (rect, reclit)
    | false -> ([%type: unit -> [%t rect]], [%expr fun () -> [%e reclit]])
  in
  let reducer ((tsofar, esofar) : (Parsetree.core_type * Parsetree.expression)) ((name, tr, _, required): Ir.field) : (Parsetree.core_type * Parsetree.expression) =
    let pname = mangle_unsafe_identifiers @@ String.uncapitalize_ascii name in
    let typ = match required with
      | false -> optionalizeType @@ type_of_tr "" tr
      | true -> type_of_tr "" tr
    in
    let pat = Gu.pat_of_name pname in
    let dflt = dflt_of_field ir (name, tr, None, required)
    in
    let lbl = match dflt with
      | None -> Asttypes.Nolabel
      | Some _ -> Asttypes.Optional pname
    in
    let e = Gu.expr_of_desc @@ Parsetree.Pexp_fun (lbl, dflt, pat, esofar) in
    let t = Gu.type_of_desc @@ Parsetree.Ptyp_arrow (lbl, typ, tsofar) in
    (t, e)
  in
  let (ft, fe) = List.fold_left reducer res fields in
  match additional with
  | None -> (ft, fe)
  | Some tr ->
    let lbl = Asttypes.Optional additionalFieldName in
    let e = Gu.expr_of_desc @@ Parsetree.Pexp_fun (lbl, (Some [%expr []]), Gu.pat_of_name additionalFieldName, fe) in
    let t = Gu.type_of_desc @@ Parsetree.Ptyp_arrow (lbl, [%type: (string * [%t type_of_tr "" tr]) list], ft) in
    (t, e)

let maker_of_ts (ir : Ir.t) (name : string) (ts : Ir.typeSpec) : (Parsetree.core_type * Parsetree.expression) option =
  match ts with
  | Ir.Record([], _) -> None
  | Ir.Record ([fld], None) -> None
  | Ir.Record (fields, additional) -> Some (maker_of_fields ir name fields additional)
  | _ -> None

let someIfOptional (required: bool) (rhs: Parsetree.expression) : Parsetree.expression = match required with
  | true -> rhs
  | false -> [%expr Some [%e rhs]]

let deoptionalize (t : Parsetree.core_type) : Parsetree.core_type = match t with
  | [%type: [%t? nopt] option] -> nopt
  | _ -> t

let setter_of_field ((fname, tr, _, required) : Ir.field) : (Parsetree.signature_item * Parsetree.structure_item) =
  let setterName = mangle_unsafe_identifiers fname in

  let arg_t = deoptionalize @@ type_of_tr "" tr in
  let sigi = [%type: [%t arg_t] -> t -> t] |> Gu.sigitem_val setterName in

  let rhs = someIfOptional required [%expr v] in
  let stri = [%expr fun v x -> [%e Gu.record_alteration [%expr x] [(setterName, rhs)]]] |> Gu.sitem_of_expr setterName in

  (sigi, stri)

let setters_of_fields (ir : Ir.t) (name : string) (fields : Ir.field list) (additional : Ir.typeRef option) : (Parsetree.signature * Parsetree.structure) =
  let append_items (sg, st) (sigi, stri) =
    (sigi :: sg, stri :: st)
  in
  let reducer ((sg, st) : (Parsetree.signature * Parsetree.structure)) ((fname, tr, _, required) : Ir.field) =
    let required = match tr with
      | Ir.AnonymousVariant ctors -> required && not (List.exists ctorWrapsNull ctors)
      | _ -> required
    in
    append_items (sg, st) @@ setter_of_field (fname, tr, None, required)
  in
  let (sg, st) = List.fold_left reducer ([], []) fields in
  match additional with
  | None -> (sg, st)
  | Some tr ->
    let sigi = [%type: (string * [%t type_of_tr "" tr]) list -> t -> t] |> Gu.sigitem_val additionalFieldName in
    let stri = [%expr fun v x -> [%e Gu.record_alteration [%expr x] [(additionalFieldName, [%expr v])]]] |> Gu.sitem_of_expr additionalFieldName in
    (sigi :: sg, stri :: st)

let setters_of_ts (ir : Ir.t) (name : string) (ts : Ir.typeSpec) : (Parsetree.signature * Parsetree.structure) =
  match ts with
  | Ir.Record ([], _) -> ([], [])
  | Ir.Record ([fld], None) -> ([], [])
  | Ir.Record (fields, additional) -> setters_of_fields ir name fields additional
  | _ -> ([], [])

let mods_of_ir (derivers: string list) (ir : Ir.t) : Parsetree.structure =
  let mod_mapper ((name, spec, desc) : (Ir.typeName * Ir.typeSpec * Ir.description)) : Parsetree.module_binding =
    let items = ([
      spec |> si_of_ts derivers;
      [%stri [%e Gu.expr_of_string ("__doc__" ^ name)]];
      to_json_of_ts spec |> Gu.sitem_of_expr "to_yojson";
    ]) in
    let sigitems = ([
      spec |> sigi_of_ts derivers;
      [%sigi: val to_yojson : t -> Yojson.Safe.json];
    ])
    in
    let maker = maker_of_ts ir name spec in
    let items_with_maker = match maker with
      | Some (_, emaker) -> let maker_item = [%stri let make = [%e emaker]] in List.concat [items; [maker_item]]
      | None -> items
    in
    let sigitems_with_maker = match maker with
      | Some (tmaker, _) -> let maker_item = [%sigi: val make : [%t tmaker]] in List.concat [sigitems; [maker_item]]
      | None -> sigitems
    in
    let (setter_sig, setter_str) = setters_of_ts ir name spec in
    let full_sig = List.concat [sigitems_with_maker; setter_sig] in
    let full_str = List.concat [items_with_maker; setter_str] in

    Gu.module_binding name full_sig full_str
  in
  ir |> List.map mod_mapper |> Gu.rec_modules

let insert_docs (ir : Ir.t) (s : string) : string =
  let reducer (sofar : string) (name, spec, desc) : string =
    let doc = doc_comment_of_desc spec desc in
    let re = Str.regexp ("[ \t]*;;\"__doc__" ^ name ^ "\"") in
    (* let _ = try
      let idx = Str.search_forward re s in
      with _ -> print_endline ("No match for "^ name)
    in *)
    Str.global_replace re doc sofar
  in
  List.fold_left reducer s ir

module Unsafe = struct
  exception DidntWork of string;;
  let unpackR (v : ('a, string) result) : 'a =
    match v with
    | Ok x -> x
    | Error s -> (
        let () = print_endline s in
        raise (DidntWork s))
end


(*
We need to shorten up constructor names, the type names can be quite long and
heavy. This makes sense for type names that need to be globally unique and
descriptive but not for constructors that are understood within the context of
their types. For example, no information is lost by replacing
`TopLevelFacetedSpec with `Faceted.
*)
exception DuplicateCtors
exception LongCtor
(* Just let this raise an exception, it's only used at codegen time. *)
let checkDuplicateCtors (ctors : Ir.ctor list) : Ir.ctor list =
  let names = List.map (fun (name, _) -> name) ctors in
  (* There's no sensible way to shorten Basis_closed/Basis_open etc., so exempt them from length check *)
  (* FIXME: SelectionDomain is being parsed all messed up, you have SelectionDomain_1 *)
  (* FIXME: In positionFieldDef, 'axis' is an 'option option'. That occurs a few times. *)
  let nonOpenClosedNames = List.filter (fun (name) -> not (Gu.regexp_matches (Str.regexp ".+_closed") name || Gu.regexp_matches (Str.regexp ".+_open") name ||  Gu.regexp_matches (Str.regexp ".+_before") name || Gu.regexp_matches (Str.regexp ".+_after") name || Gu.regexp_matches (Str.regexp ".+_linear") name || Gu.regexp_matches (Str.regexp ".+_ordinal") name || String.equal name "SelectionDomain" || String.equal name "SelectionDomain_1" || String.equal name "Quantitative")) names in
  let _ = if List.length ctors != List.length @@ List.sort_uniq String.compare names then
      let _ = print_endline ("Duplicate constructor names " ^ (String.concat ", " names)) in
      raise DuplicateCtors
    else if List.exists (fun name -> String.length name > 10) nonOpenClosedNames then
      let _ = print_endline ("Long constructor name " ^ (String.concat ", " nonOpenClosedNames)) in
      (* raise LongCtor *)
      ()
    else ()
  in
  ctors

let replace_match (res : string) (wth : string) (name : string) : string =
  if Gu.regexp_matches (Str.regexp res) name then Str.replace_first (Str.regexp res) wth name else name

let kill_suffix (suf : string) (name : string) : string =
  if Gu.regexp_matches (Str.regexp @@ ".+" ^ suf) name then Str.replace_first (Str.regexp suf) "" name else name

let kill_prefix (pref : string) (name : string) : string =
  if Gu.regexp_matches (Str.regexp @@ pref ^ ".+") name then Str.replace_first (Str.regexp pref) "" name else name

let replace_if_match (res : string) (v : string) (name : string) : string =
  if Gu.regexp_matches (Str.regexp res) name then v else name

let rec nicknameTrCtors (tr : Ir.typeRef) : Ir.typeRef =
  let open Ir in
  match tr with
  | AnonymousVariant ctors -> AnonymousVariant (checkDuplicateCtors @@ List.map nicknameCtor ctors)
  | List tr -> List (nicknameTrCtors tr)
  | x -> x

and nicknameCtor ((name, trs) : Ir.ctor) : Ir.ctor =
  if (List.length trs) = 0 then
    (* Don't rename enums, as they occur in the spec. *)
    (name, trs)
  else let nickname =
    if Gu.regexp_matches (Str.regexp "Field") name then match trs with
      | [Ir.List _] -> "Fields"
      | _ -> "Field"
    else name
     |> replace_if_match "Value" "Value"
     |> replace_if_match "Number" "Num"
     |> replace_if_match "Params" "Params"
     |> replace_if_match "CompositeUnit" "Unit"
     |> replace_if_match "Independent" "Ind"
     |> replace_if_match "Interpolate" "Interp"
     |> replace_if_match "Unaggregated" "Unagg"
     (* |> replace_if_match "SelectionDomain" "SelDom" *)
     |> replace_if_match "Local" "Local"
     |> replace_match "Bottom" "Bot"
     |> replace_match "[Mm]illiseconds" "Ms"
     |> replace_match "[Ss]econds" "S"
     |> replace_match "[Mm]inutes" "Min"
     |> replace_match "[Hh]ours" "H"
     |> replace_match "[Mm]onth" "Mo"
     |> replace_match "date" "D"
     |> replace_match "[Qq]uarter" "Q"
     |> replace_match "[Yy]ear" "Y"
     |> replace_match "Utc" "U"
     |> replace_match "_right" "R"
     |> replace_match "_left" "L"
     |> kill_suffix "Ref"
     |> kill_suffix "Op"
     |> kill_suffix "DataFormat"
     |> kill_suffix "Data"
     |> kill_suffix "Spec"
     |> kill_suffix "Filter"
     |> kill_suffix "Selection"
     |> kill_prefix "Selection"
     |> kill_suffix "_Bind"
     |> kill_suffix "Binding"
     |> kill_suffix "_Config"
     |> kill_suffix "Config"
     |> kill_suffix "TimeUnit"
     |> kill_prefix "TopLevel"
     |> replace_if_match "FacetedUnit" "Unit"
     |> kill_suffix "Transform"
     |> kill_prefix "Sort"
  in
  (nickname, List.map nicknameTrCtors trs)

let nicknameIrCtors (ir : Ir.t) : Ir.t =
  let open Ir in
  let mapper (name, ts, desc) =
    let ts_ = match ts with
      | Variant cl -> Variant (checkDuplicateCtors @@ List.map nicknameCtor cl)
      | Alias x -> Alias x
      | Record (flds, None) -> Record (List.map (fun (name, tr, desc, req) -> (name, nicknameTrCtors tr, desc, req)) flds, None)
      | Record (flds, Some tr) -> Record (List.map (fun (name, tr, desc, req) -> (name, nicknameTrCtors tr, desc, req)) flds, Some (nicknameTrCtors tr))
    in
    (name, ts_, desc)
  in
  List.map mapper ir

(* We need to get rid of CompositeUnitSpecAlias and FacetedCompositeUnitSpecAlias for simplicity *)
let rec renameTypeTr (fromName : string) (toName : string) (tr : Ir.typeRef) : Ir.typeRef =
  let open Ir in
  match tr with
  | List tr_ -> List (renameTypeTr fromName toName tr_)
  | Ref s -> Ref (if (String.equal fromName s) then toName else (Str.replace_first (Str.regexp fromName) toName s))
  | AnonymousVariant cl -> (AnonymousVariant (List.map (fun (name, trs) ->
      (name, List.map (renameTypeTr fromName toName) trs)) cl))
  | x -> x

let renameType (fromName : string) (toName : string) (ir : Ir.t) : Ir.t =
  let open Ir in
  let reducer sofar (name, ts, desc) =
    (* If the thing you're renaming to is there, drop it *)
    if (String.equal name toName) then sofar
    else let ts_ = match ts with
      | Alias tr -> (Alias (renameTypeTr fromName toName tr))
      | Variant cl -> (Variant (List.map (fun (name, trs) ->
          (name, List.map (renameTypeTr fromName toName) trs)) cl))
      | Record (flds, None) -> (Record (List.map (fun (name, tr, desc, req) ->
          (name, renameTypeTr fromName toName tr, desc, req)) flds, None))
      | Record (flds, Some tr) -> (Record (List.map (fun (name, tr, desc, req) ->
          (name, renameTypeTr fromName toName tr, desc, req)) flds, Some (renameTypeTr fromName toName tr)))
      in
      let name_ = if (String.equal name fromName) then toName
        else Str.replace_first (Str.regexp fromName) toName name in
      (name_, ts_, desc) :: sofar
  in
  List.fold_left reducer [] ir

let squashAliases (ir : Ir.t) : Ir.t =
  let open Ir in
  ir
  |> renameType "FacetedCompositeUnitSpecAlias" "FacetedCompositeUnitSpec"
  |> renameType "CompositeUnitSpecAlias" "CompositeUnitSpec"

let squashSingletonCtors (ctors: Ir.ctor list) : Ir.typeRef option =
  match ctors with
  | [(ctorName, [ctorTR])] -> Some ctorTR
  | [(ctorName, [ctorTR]); (_, [Ir.Null])] -> Some ctorTR
  | [(_, [Ir.Null]); (ctorName, [ctorTR])] -> Some ctorTR
  | _ -> None

let squashSingletonVariants (ir : Ir.t) : Ir.t =
  let open Ir in
  let reducer sofar (name, ts, desc) =
    match ts with
    | Variant ctors -> (match squashSingletonCtors ctors with
        | Some tr ->
          (name, (Alias tr), desc) :: sofar
      | None -> (name, ts, desc) :: sofar)
    | Record (fields, additional) ->
      (* let _ = match fields with
        | [fld] -> print_endline ("Singleton record: " ^ name)
        | _ -> ()
         in *)
      let trTransformer tr = match tr with
        | AnonymousVariant ctors ->
          (match squashSingletonCtors ctors with
           | Some tr_ -> tr_
           | None -> tr)
        | _ -> tr
      in

      let mapper (fname, tr, desc, req) = (fname, trTransformer tr, desc, req) in
      let additional_ = match additional with
        | None -> None
        | Some tr -> Some (trTransformer tr)
      in
      (name, Record (List.map mapper fields, additional_), desc) :: sofar
    | _ -> (name, ts, desc) :: sofar
  in
  List.fold_left reducer [] ir

let squashNumCtors (ctors: Ir.ctor list) : Ir.ctor list =
  let open Ir in
  let reducer (sofar: Ir.ctor list) (next : Ir.ctor) =
    match next with
    | ("Num", [Num _]) -> ("Float", [Float]) :: ("Int", [Int]) :: sofar
    | _ -> next :: sofar
  in
  List.fold_left reducer [] ctors

let squashNumVariants (ir : Ir.t) : Ir.t =
  let open Ir in
  let reducer sofar (name, ts, desc) =
    match ts with
    | Variant ctors -> (name, Variant (squashNumCtors ctors), desc) :: sofar
    | Record (fields, additional) ->
      let trTransformer tr = match tr with
        | AnonymousVariant ctors -> AnonymousVariant (squashNumCtors ctors)
        | _ -> tr
      in
      let mapper (fname, tr, desc, req) = (fname, trTransformer tr, desc, req) in
      let additional_ = match additional with
        | None -> None
        | Some tr -> Some (trTransformer tr)
      in
      (name, Record (List.map mapper fields, additional_), desc) :: sofar
    | _ -> (name, ts, desc) :: sofar
  in
  List.fold_left reducer [] ir

let () =
  let open Unsafe in
  let derivers = Array.to_list Sys.argv |> List.tl in
  let schema = Yojson.Safe.from_file "./v2.0.0-rc2.json" in
  let ir = Ir.parse schema |> unpackR |> squashSingletonVariants |> squashNumVariants |> squashAliases |>  nicknameIrCtors in
  let code = ir |> mods_of_ir derivers |> Gu.string_of_struct |> Gu.dedent_mutrec_mods |> insert_docs ir |> fun (code) -> ("(* Generated by gen/gen.ml *)\n\nmodule V2 = struct\n" ^ code ^ "\nend\n") in
  (*
    NOTE: The contsructors have to be emitted outside the modules rather than in
    a 'make' attribute because the modules have to have explicit signatures because
    they're mutually recursive, but there's no consistent type for 'make' because
    the constructors take all kinds of different parameters.
  *)
  let () = save_file "../vegaLite.ml" code in
  ()

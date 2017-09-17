(* TODO:
   - Generate an explicit signature per module to export the record type
   - Move the 'makers' into the modules, put them in the explicit signatures
*)
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

let rec type_of_tr (tr : Ir.typeRef) : Parsetree.core_type =
  let open Ir in
  match tr with
  | Num (min, max) -> [%type: int] (* TODO: distinguish ints from floats? *)
  | String -> [%type: string]
  | Bool -> [%type: bool]
  | JSON -> [%type: Yojson.Safe.json] (* Note, when 'object' occurs in type references it means 'any'. We represent this as Yojson *)
  | Ref s -> Gu.simple_type (s ^ ".t")
  | List el -> [%type: [%t type_of_tr el] list]
  | Null -> raise NullType

let rf_of_ctor ((name, trs) : Ir.ctor) : Parsetree.row_field =
  let typs = List.map type_of_tr trs in
  Parsetree.Rtag (name, [], false, typs)

let ctorWrapsNull = function
  | (_, [Ir.Null]) -> true
  | _ -> false
let ctorNotWrapsNull c = match ctorWrapsNull c with
  | true -> false
  | false -> true
let type_of_ctors (ctors: Ir.ctor list) : Parsetree.core_type =
  let isOption = List.exists ctorWrapsNull ctors in
  let rfs = List.map rf_of_ctor (List.filter ctorNotWrapsNull ctors) in
  let typ = Gu.type_of_desc @@ Parsetree.Ptyp_variant (rfs, Asttypes.Closed, None) in
  match isOption with
  | true -> [%type: [%t typ] option]
  | false -> typ

let ld_of_field ((name, tr, _, required) : Ir.field) : Parsetree.label_declaration =
  let pname = mangle_unsafe_identifiers name in
  let typ = match required with
    | true -> type_of_tr tr
    | false -> [%type: [%t (type_of_tr tr)] option]
  in
  Gu.label_decl pname typ

let tdecl_of_fields (fields: Ir.field list) : Parsetree.type_declaration =
  let lds = List.map ld_of_field fields in
  Gu.rectype_of_labels "t" lds

let si_of_ts (spec: Ir.typeSpec) : Parsetree.structure_item =
  match spec with
  | Ir.Alias tr -> [%stri type t = [%t type_of_tr tr]]
    (*
    Note: if you want these to be non-backticked variants, you need to build a
    type_declaration wrapping a Ptype_variant.
    *)
  | Ir.Variant ctors -> [%stri type t = [%t type_of_ctors ctors]]
  | Ir.Record fields -> Gu.sitem_of_tdecl @@ tdecl_of_fields fields

let sigi_of_ts (spec: Ir.typeSpec) : Parsetree.signature_item =
  match spec with
  | Ir.Alias tr -> [%sigi: type t = [%t type_of_tr tr]]
    (*
    Note: if you want these to be non-backticked variants, you need to build a
    type_declaration wrapping a Ptype_variant.
    *)
  | Ir.Variant ctors -> [%sigi: type t = [%t type_of_ctors ctors]]
  | Ir.Record fields -> Gu.sigitem_of_tdecl @@ tdecl_of_fields fields


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
    | Ir.Record fields -> (
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

let rec to_json_of_tr (tr : Ir.typeRef) : Parsetree.expression =
  let open Ir in
  match tr with
  | Num (min, max) -> [%expr fun (i : int) -> (`Int i : Yojson.Safe.json)]
  | String -> [%expr fun (s : string) -> (`String s : Yojson.Safe.json)]
  | Bool -> [%expr fun (b : bool) -> (`Bool b : Yojson.Safe.json)]
  | JSON -> [%expr fun (j : Yojson.Safe.json) -> (j : Yojson.Safe.json)]
  | Ref s -> [%expr fun (x : [%t Gu.simple_type (s ^ ".t")]) -> (([%e Gu.ident_expr (s ^ ".to_yojson")] x) : Yojson.Safe.json)]
  | List el -> [%expr fun (l : [%t type_of_tr el] list) ->
    let yojson_of_el= [%e to_json_of_tr el]  in
    (`List (List.map yojson_of_el l) : Yojson.Safe.json)
  ]
  | Null -> raise NullType

let to_json_of_fields (fields : Ir.field list) : Parsetree.expression =
  let el_of_field ((name, tr, _, required) : Ir.field) : Parsetree.expression =
    let pname = mangle_unsafe_identifiers name in
    let valExpr = Gu.ident_expr ("x." ^ pname) in
    let jsonVal = match required with
      | true -> [%expr [%e to_json_of_tr tr] [%e valExpr]]
      | false -> [%expr (function
          | None -> `Null
          | Some v -> [%e to_json_of_tr tr] v) [%e valExpr]]
    in
    (* Use the unmangled, unsafe name here in the json. *)
    [%expr ([%e Gu.expr_of_string name], [%e jsonVal])]
  in
  let reducer (sofar : Parsetree.expression) (next : Ir.field) : Parsetree.expression =
    [%expr [%e el_of_field next] :: [%e sofar]]
  in
  let contents = List.fold_left reducer [%expr []] fields in
  [%expr fun x -> `Assoc (List.filter (fun (k, v) -> match v with | `Null -> false | _ -> true) [%e contents])]

let to_json_of_ctors (ctors : Ir.ctor list) : Parsetree.expression =
  let isOption = List.exists ctorWrapsNull ctors in
  let case_of_opt_ctor ((name, trs) : Ir.ctor) : Parsetree.case =
    let sname = String.uncapitalize_ascii name in
    match trs with
    (* In the case of a flat variant, it needs to be translated to a string. *)
    | [] -> Gu.case_of_pat_and_expr (Gu.variant name |> Gu.construct "Some") [%expr `String [%e Gu.expr_of_string sname]]
    | fst :: snd :: _ -> let () = print_endline name in raise NullType
    | Ir.Null :: [] -> Gu.case_of_pat_and_expr (Gu.pat_of_name "None") [%expr `Null]
    | tr :: [] ->
      Gu.case_of_pat_and_expr (Gu.pat_of_name "v" |> Gu.variant_with_arg name |> Gu.construct "Some") [%expr [%e to_json_of_tr tr] [%e Gu.ident_expr "v"]]
  in
  let case_of_req_ctor ((name, trs) : Ir.ctor) : Parsetree.case =
    let sname = String.uncapitalize_ascii name in
    match trs with
    (* In the case of a flat variant, it needs to be translated to a string. *)
    | [] -> Gu.case_of_pat_and_expr (Gu.variant name) [%expr `String [%e Gu.expr_of_string sname]]
    | fst :: snd :: _ -> let () = print_endline name in raise NullType
    | tr :: [] ->
      Gu.case_of_pat_and_expr (Gu.pat_of_name "v" |> Gu.variant_with_arg name) [%expr [%e to_json_of_tr tr] [%e Gu.ident_expr "v"]]
  in
  let case_of_ctor (ctor : Ir.ctor) : Parsetree.case =
    match isOption with
    | true -> case_of_opt_ctor ctor
    | false -> case_of_req_ctor ctor
  in
  Gu.expr_of_desc @@ Parsetree.Pexp_function (List.map case_of_ctor ctors)

let to_json_of_ts (ts : Ir.typeSpec) : Parsetree.expression =
  match ts with
  | Ir.Alias tr -> to_json_of_tr tr
  | Ir.Record fields -> to_json_of_fields fields
  | Ir.Variant ctors -> to_json_of_ctors ctors


let rec of_json_of_tr (tr : Ir.typeRef) : Parsetree.expression =
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
  | Ir.Variant ctors -> of_json_of_ctors ctors


let record_literal (fields: Ir.field list) : Parsetree.expression =
  let mapper ((fieldName, _, _, _) : Ir.field) : (Longident.t Asttypes.loc * Parsetree.expression) =
    let sfn = mangle_unsafe_identifiers fieldName in
    (Gu.ident_of_name sfn, Gu.ident_expr sfn)
  in
  Gu.expr_of_desc @@ Parsetree.Pexp_record (List.map mapper fields, None)

let maker_of_fields (mname : string) (fields: Ir.field list) : (Parsetree.core_type * Parsetree.expression) =
  let fcomp (n1, _, _, r1) (n2, _, _, r2) =
    match (r1, r2) with
    | (true, false) -> -1
    | (false, true) -> 1
    | _ -> String.compare n1 n2
  in
  let some_required = List.exists (fun (_, _, _, r) -> r) fields in
  let fields = List.sort fcomp fields in
  let reclit = record_literal fields in
  let rect = Gu.simple_type "t" in
  let res = match some_required with
    | true -> (rect, reclit)
    | false -> ([%type: unit -> [%t rect]], [%expr fun () -> [%e reclit]])
  in
  let reducer ((tsofar, esofar) : (Parsetree.core_type * Parsetree.expression)) ((name, tr, _, required): Ir.field) : (Parsetree.core_type * Parsetree.expression) =
    let pname = mangle_unsafe_identifiers @@ String.uncapitalize_ascii name in
    let typ = match required with
      | false -> [%type: [%t type_of_tr tr] option]
      | true -> type_of_tr tr
    in
    let pat = Gu.pat_of_name pname in
    let lbl = match required with
      | true -> Asttypes.Nolabel
      | false -> Asttypes.Optional pname
    in
    let dflt = match (name, required) with
      | ("$schema", true) -> Some [%expr "https://vega.github.io/schema/vega-lite/v2.json"]
      | ("$schema", false) -> Some [%expr Some "https://vega.github.io/schema/vega-lite/v2.json"]
      | (_, true) -> None
      | (_, false) -> Some [%expr None]
    in
    let e = Gu.expr_of_desc @@ Parsetree.Pexp_fun (lbl, dflt, pat, esofar) in
    let t = Gu.type_of_desc @@ Parsetree.Ptyp_arrow (lbl, typ, tsofar) in
    (t, e)
  in
  List.fold_left reducer res fields

let maker_of_ts (name : string) (ts : Ir.typeSpec) : (Parsetree.core_type * Parsetree.expression) option =
  match ts with
  | Ir.Record fields -> Some (maker_of_fields name fields)
  | _ -> None

(* TODO: Add a unit argument to every constructor fn if all are optional. Move non-optional to last. *)
(* TODO: Type sig syntax with optional args is
   let f : ?x:(int option) -> unit -> int option = fun ?(x=(Some 0: int option)) () -> x
*)

let mods_of_ir (ir : Ir.t) : Parsetree.structure =
  let mod_mapper ((name, spec, desc) : (Ir.typeName * Ir.typeSpec * Ir.description)) : Parsetree.module_binding =
    let items = [
      spec |> si_of_ts;
      [%stri [%e Gu.expr_of_string ("__doc__" ^ name)]];
      to_json_of_ts spec |> Gu.sitem_of_expr "to_yojson";
    ] in
    let sigitems = [
      spec |> sigi_of_ts;
      [%sigi: val to_yojson : t -> Yojson.Safe.json];
    ]
    in
    let maker = maker_of_ts name spec in
    let items_with_maker = match maker with
      | Some (_, emaker) -> let maker_item = [%stri let make = [%e emaker]] in List.concat [items; [maker_item]]
      | None -> items
    in
    let sigitems_with_maker = match maker with
      | Some (tmaker, _) -> let maker_item = [%sigi: val make : [%t tmaker]] in List.concat [sigitems; [maker_item]]
      | None -> sigitems
    in
    Gu.module_binding name sigitems_with_maker items_with_maker
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

let () =
  let open Unsafe in
  let schema = Yojson.Safe.from_file "./v2.0.0-rc2.json" in
  let ir = Ir.parse schema |> unpackR in
  let code = ir |> mods_of_ir |> Gu.string_of_struct |> Gu.dedent_mutrec_mods |> insert_docs ir in
  (*
    NOTE: The contsructors have to be emitted outside the modules rather than in
    a 'make' attribute because the modules have to have explicit signatures because
    they're mutually recursive, but there's no consistent type for 'make' because
    the constructors take all kinds of different parameters.
  *)
  save_file "../vegaLite.ml" ("(* Generated by gen/gen.ml *)\n\nmodule V2 = struct\n" ^ code ^ "\nend\n")


(* TODO: Call the high-level interactive lib 'aplomb'. You'll need to write a livereload server in ocaml. *)

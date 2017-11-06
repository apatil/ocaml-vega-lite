(*
Utility to wrap an expression generated with [%expr blah] in a 'description',
which is needed to embed the expression in some P structures.
*)
let expr_of_desc (desc : Parsetree.expression_desc) : Parsetree.expression =
  Parsetree.{
    pexp_desc = desc;
    pexp_loc = Location.none;
    pexp_attributes = []
  };;

(* Like expr_of_desc for patterns *)
let pattern_of_desc (desc : Parsetree.pattern_desc) : Parsetree.pattern =
  Parsetree.{
    ppat_desc = desc;
    ppat_loc = Location.none;
    ppat_attributes = [];
  };;

(* Like expr_of_desc for types *)
let type_of_desc (desc: Parsetree.core_type_desc) : Parsetree.core_type =
  Parsetree.{
    ptyp_desc = desc;
    ptyp_loc = Location.none;
    ptyp_attributes = [];
  }

(* Like expr_of_desc for module types *)
let mtype_of_desc (desc : Parsetree.module_type_desc) : Parsetree.module_type =
  Parsetree.{
    pmty_desc = desc;
    pmty_loc = Location.none;
    pmty_attributes = [];
  }

(* Converts an LHS and an RHS to a value binding, suitable for a let expression and other things *)
let value_binding (lhs : Parsetree.pattern) (rhs : Parsetree.expression) : Parsetree.value_binding =
  Parsetree.{
    pvb_pat = lhs;
    pvb_expr = rhs;
    pvb_attributes = [];
    pvb_loc = Location.none;
  }

(* Convert a string to an identifier *)
let ident_of_name (name : string) : Longident.t Asttypes.loc =
  Asttypes.{txt = Longident.Lident name; loc = Location.none}

(* Convert a string to a type reference with no type parameters. *)
let simple_type (typ : string) : Parsetree.core_type =
  type_of_desc @@ Parsetree.Ptyp_constr (ident_of_name typ, [])

(* Convert a string to an expression consisting of a single identifier *)
let ident_expr (name : string) : Parsetree.expression =
  expr_of_desc @@ Parsetree.Pexp_ident (ident_of_name name)

(* Convert a string to a signature reference. *)
let simple_mtype (syg : string) : Parsetree.module_type =
  mtype_of_desc @@ Parsetree.Pmty_ident (ident_of_name syg)

(*
The LHS of a pattern match, from a string. This is a variable as opposed to a
variant constructor, eg y in
match x with
| y -> 0
*)
let pat_of_name (name : string) : Parsetree.pattern =
  pattern_of_desc @@ Parsetree.Ppat_var Asttypes.{txt = name; loc = Location.none}

let constraint_of_name_and_type (name : string) (typ : Parsetree.core_type) : Parsetree.pattern =
  pattern_of_desc @@ Parsetree.Ppat_constraint (pat_of_name name, typ)

(* Converts a list of types to a tuple type *)
let tuple_type (typs : Parsetree.core_type list) : Parsetree.core_type =
  type_of_desc @@ Parsetree.Ptyp_tuple typs

(* Gets a dot expression, eg x.y *)
let dot (e : Parsetree.expression) (attr :  string) : Parsetree.expression =
  expr_of_desc @@ Parsetree.Pexp_field (e, ident_of_name attr)

(* Generic record literal expression *)
let record_literal (base : Parsetree.expression option) (fields : (string * Parsetree.expression) list) : Parsetree.expression =
  let mapper (fieldName, fieldVal) : (Longident.t Asttypes.loc * Parsetree.expression) =
    (ident_of_name fieldName, fieldVal)
  in
  expr_of_desc @@ Parsetree.Pexp_record (List.map mapper fields, base)

(* Gets a record alteration expression, eg {x with k = v} *)
let record_alteration (base : Parsetree.expression) (fields : (string * Parsetree.expression) list) : Parsetree.expression =
  record_literal (Some base) fields

(* Gets a variant expression, eg `Hi x*)
let variant_expr (name : string) (param : Parsetree.expression option) : Parsetree.expression =
  expr_of_desc @@ Parsetree.Pexp_variant (name, param)

let tuple_of_exprs (exprs : Parsetree.expression list) : Parsetree.expression =
  expr_of_desc @@ Parsetree.Pexp_tuple exprs

let deriver_attributes (derivers : string list) : Parsetree.attributes =
  let open Parsetree in
  match derivers with
  | [] -> []
  | _ ->
    let deriver_exprs = List.map ident_expr derivers in
    [(
      Asttypes.{txt = "deriving"; loc = Location.none},
      PStr [%str [%e tuple_of_exprs deriver_exprs]]
        (* [
          structure_item (stx.ml[1,0+35]..[1,0+47])
            Pstr_eval
            expression (stx.ml[1,0+35]..[1,0+47])
            Pexp_tuple
            [
              expression (stx.ml[1,0+35]..[1,0+39])
                Pexp_ident "lens" (stx.ml[1,0+35]..[1,0+39])
                expression (stx.ml[1,0+41]..[1,0+47])
                Pexp_ident "yojson" (stx.ml[1,0+41]..[1,0+47])
            ]
        ] *)
    )]

(*
Converts a P type_kind, which is 'abstract', 'open', 'record' or 'variant'
into a type declaration with a name.
*)
let type_decl_of_kind (derivers : string list) (name : string) (kind : Parsetree.type_kind) : Parsetree.type_declaration =
  Parsetree.{
    ptype_name = Asttypes.{txt = name; loc = Location.none};
    ptype_params = [];
    ptype_cstrs = [];
    ptype_kind = kind;
    ptype_private = Asttypes.Public;
    ptype_manifest = None;
    ptype_attributes = deriver_attributes derivers;
    ptype_loc = Location.none;
  }

let type_decl_of_type (derivers : string list) (name : string) (typ : Parsetree.core_type) : Parsetree.type_declaration =
  Parsetree.{
    ptype_name = Asttypes.{txt = name; loc = Location.none};
    ptype_params = [];
    ptype_cstrs = [];
    ptype_kind = Ptype_abstract;
    ptype_private = Asttypes.Public;
    ptype_manifest = (Some typ);
    ptype_attributes = deriver_attributes derivers;
    ptype_loc = Location.none;
  }


let wrap_in_open (name : string) (expr : Parsetree.expression) : Parsetree.expression =
  expr_of_desc @@ Parsetree.Pexp_open (Asttypes.Fresh, ident_of_name name, expr)

(*
Create a label declaration suitable for use in a record type from a field name
and a type.
*)
let label_decl (name : string) (typ : Parsetree.core_type) : Parsetree.label_declaration =
  Parsetree.{
    pld_name = Location.{txt = name; loc = Location.none};
    pld_mutable = Asttypes.Immutable;
    pld_type = typ;
    pld_loc = Location.none;
    pld_attributes = [];
  }

(* Create a record type declaration from label declarations and a type name. *)
let rectype_of_labels (derivers: string list) (name : string) (labels : Parsetree.label_declaration list) : Parsetree.type_declaration =
  type_decl_of_kind derivers name @@ Parsetree.Ptype_record labels

(* Create a single-argument function expression. *)
let fun_expr (argname : string) (intyp: Parsetree.core_type) (rhs : Parsetree.expression) (outtyp : Parsetree.core_type option) : Parsetree.expression =
  let open Parsetree in
  let argvar = pat_of_name argname in
  let lhs = [%pat? ([%p argvar] : [%t intyp])] in
  let rhs = match outtyp with
    | Some typ -> [%expr ([%e rhs] : [%t typ])]
    | None -> rhs
  in
  [%expr fun ([%p lhs]) -> [%e rhs]]

(* Convert a boolean value to its corresponding expression. *)
let expr_of_bool (b : bool) : Parsetree.expression = match b with
  | true -> [%expr true]
  | false -> [%expr false]

(* Convert a float value to its corresponding expression. *)
let expr_of_float (f : float) : Parsetree.expression =
  let open Parsetree in
  expr_of_desc @@ Pexp_constant (Pconst_float (string_of_float f, None))

(* Convert an int value to its corresponding expression. *)
let expr_of_int (i : int) : Parsetree.expression =
  let open Parsetree in
  expr_of_desc @@ Pexp_constant (Pconst_integer (string_of_int i, None))

(* Convert a string value to its corresponding expression. *)
let expr_of_string (s : string) : Parsetree.expression =
  let open Parsetree in
  expr_of_desc @@ Pexp_constant (Pconst_string (s, None))

(* Convert a yojson value to its corresponding expression. *)
let rec expr_of_yojson (j : Yojson.Basic.json) : Parsetree.expression =
  let open Parsetree in
  match j with
  | `Bool b ->
    (* [%expr `Null]     *)
    [%expr `Bool [%e (expr_of_bool b)]]
  | `Float f ->
    (* [%expr `Null] *)
    [%expr `Float [%e (expr_of_float f)]]
  | `Int i ->
    (* [%expr `Null] *)
    [%expr `Int [%e (expr_of_int i)]]
  | `String s ->
    (* [%expr `Null] *)
    [%expr `String [%e (expr_of_string s)]]
  | `Null -> [%expr `Null]
  | `List l ->
    (* [%expr `Null] *)
    let reducer sofar next =
      [%expr [%e expr_of_yojson next] :: [%e sofar]]
    in
    [%expr `List [%e List.fold_left reducer [%expr []] l]]
  | `Assoc l ->
    (* [%expr `Null] *)
    let reducer sofar (k, v) =
      [%expr ([%e expr_of_string k], [%e expr_of_yojson v]) :: [%e sofar]]
    in
    [%expr `Assoc [%e List.fold_left reducer [%expr []] l]]

let case_of_pat_and_expr (pat : Parsetree.pattern) (rhs : Parsetree.expression) : Parsetree.case =
  Parsetree.{
    pc_lhs = pat;
    pc_guard = None;
    pc_rhs = rhs
  }

(* Wraps a pattern 'x' in 'Vname x' *)
let construct (vname : string) (argument : Parsetree.pattern) : Parsetree.pattern =
  pattern_of_desc @@ Parsetree.Ppat_construct (ident_of_name vname, Some argument)

(* Wraps a pattern 'x' in '`Vname x' with a backtick *)
let variant (vname : string) : Parsetree.pattern =
  pattern_of_desc @@ Parsetree.Ppat_variant (vname, None)
let variant_with_arg (vname : string) (argument : Parsetree.pattern) : Parsetree.pattern =
  pattern_of_desc @@ Parsetree.Ppat_variant (vname, Some argument)

(* Wraps a type in the 'option' version of it. *)
let to_option_type (t : Parsetree.core_type) : Parsetree.core_type =
  [%type: [%t t] option]

(* Wraps a type in the string-result version of it. *)
let to_result_type (t : Parsetree.core_type) : Parsetree.core_type =
  [%type: ([%t t], string) result]

(* Wraps a sequence of structure items in a module declaration.*)
let module_of_sitems (modname : string) (items : Parsetree.structure_item list) =
  [%str [%%s items]]

let sigitem_of_desc (desc: Parsetree.signature_item_desc) : Parsetree.signature_item =
  Parsetree.{
    psig_desc = desc;
    psig_loc = Location.none;
  }

let sigitem_val (name : string) (typ : Parsetree.core_type) : Parsetree.signature_item =
  let valdesc = Parsetree.{
      pval_name = Asttypes.{txt = name; loc = Location.none};
      pval_type = typ;
      pval_prim = [];
      pval_attributes = [];
      pval_loc = Location.none;
    }
  in
  sigitem_of_desc @@ Parsetree.Psig_value valdesc

let sigitem_of_tdecl (decl: Parsetree.type_declaration) : Parsetree.signature_item =
  sigitem_of_desc @@ Parsetree.Psig_type (Asttypes.Nonrecursive, [decl])

(*
Wraps an expression and its binding in a structure item, eg
sitem_of_expr [%pat? x] [%expr 3] -> "let x = 3"
*)
let sitem_of_desc (desc: Parsetree.structure_item_desc) : Parsetree.structure_item =
  Parsetree.{
    pstr_desc = desc;
    pstr_loc = Location.none;
  }

let sitem_of_expr (name : string) (rhs : Parsetree.expression) : Parsetree.structure_item =
  let pat = pat_of_name name in
  [%stri let [%p pat] = [%e rhs]]

(* Wraps a type declaration in a module structure item *)
let sitem_of_tdecl (decl: Parsetree.type_declaration) : Parsetree.structure_item =
  sitem_of_desc @@ Parsetree.Pstr_type (Asttypes.Nonrecursive, [decl])

let sitem_of_mb (mb : Parsetree.module_binding) : Parsetree.structure_item =
  sitem_of_desc @@ Parsetree.Pstr_module mb

let modtype_of_sig (sg : Parsetree.signature) : Parsetree.module_type =
  Parsetree.{
    pmty_desc = Pmty_signature sg;
    pmty_loc = Location.none;
    pmty_attributes = [];
  }

(* Wraps up a list of structure items into a module binding. *)
let module_binding (name : string) (sg : Parsetree.signature) (rhs : Parsetree.structure) : Parsetree.module_binding =
  let open Parsetree in
  let rhs_t = modtype_of_sig sg in
  let me : module_expr = {
      pmod_desc = Pmod_structure rhs;
      pmod_loc = Location.none;
      pmod_attributes = []
    };
  in
  {
    pmb_name = Asttypes.{txt=name; loc=Location.none};
    pmb_expr = {
      pmod_desc = Pmod_constraint (me, rhs_t);
      pmod_loc = Location.none;
      pmod_attributes = []
    };
    pmb_attributes = [];
    pmb_loc = Location.none
  }



(*
Bundles up a list of module bindings into a sequence of recursive modules, suitable
for dumping with string_of_items.
*)
let rec_modules (mods: Parsetree.module_binding list) : Parsetree.structure =
  [
    Parsetree.{
      pstr_loc = Location.none;
      pstr_desc = Pstr_recmodule mods
    }
  ]

(*
Pprintast dumps mutually recursive modules at ever increasing indent levels, un-f
it. This is horrible, is there a better way?
*)
let dedent_mutrec_mods (raw : string) : string =
  let lines = Str.split (Str.regexp "\n") raw in
  let isModFence (line : string) : bool =
    let re = Str.regexp "[ \t]*end" in
    Str.string_match re line 0
  in
  let numLeadingSpaces (line : string) : int =
    let re = (Str.regexp "[^ \t]") in
    try
      Str.search_forward re line 0
    with _ -> 0
  in
  let shouldDedentFull (line : string) : bool =
    try
      let _ = Str.search_forward (Str.regexp "^[ \t]*struct") line 0 in
      true
    with _ -> try
      let _ = Str.search_forward (Str.regexp "^[ \t]*end") line 0 in
      true
    with _ -> try
      let _ = Str.search_forward (Str.regexp "^[ \t]*and") line 0 in
      true
    with _ -> try
      let _ = Str.search_forward (Str.regexp "^[ \t]*[A-Z][a-zA-z: =]+") line 0 in
      true
    with _ -> false
  in
  let shouldSquash (next : string) (prev : string) : bool =
    try
      let _ = Str.search_forward (Str.regexp "^[ \t]*=") prev 0 in
      try
        let _ = Str.search_forward (Str.regexp "^[ \t]*struct") next 0 in
        true
      with _ -> false
    with _ -> try
      let _ = Str.search_forward (Str.regexp "^[ \t]*$") next 0 in
      true
    with _ -> false
  in
  let squash (next : string) (prev : string) : string =
    (String.trim prev) ^ " " ^ next
  in
  let dedent (numSpaces : int) (line : string) : string =
    let leading : int = numLeadingSpaces line in
    match shouldDedentFull line with
    | true -> Str.string_after line leading
    | false -> (match leading > numSpaces with
        | true -> Str.string_after line numSpaces
        | false -> line
      )
  in
  let reducer ((dedented, spcs) : (string list * int)) (next : string) : (string list * int) =
    match isModFence next with
    | false -> (
        match dedented with
        | hd :: tl -> (match shouldSquash hd next with
            | false -> ((dedent spcs next) :: dedented, spcs)
            | true -> ((squash hd next) :: tl, spcs))
        | [] -> ([dedent spcs next], spcs))
    | true -> (
        let spcs_ = numLeadingSpaces next in
        ((dedent spcs_ next) :: dedented, spcs_)
      )
  in
  let (dedented, _) = List.fold_left reducer ([], 0) (List.rev lines) in
  String.concat "\n" dedented


(* Dumps a module to code *)
let string_of_struct (items : Parsetree.structure) : string =
  Pprintast.string_of_structure items

let regexp_matches (re : Str.regexp) (s : string) : bool =
  try
    let _ = Str.search_forward re s 0 in true
  with
  | _ -> false

let keep_somes (l : 'a option list) : 'a list =
  List.fold_left (fun sofar next -> match next with
      | None -> sofar
      | Some x -> x :: sofar) [] l

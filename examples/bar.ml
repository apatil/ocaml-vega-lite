(* https://vega.github.io/vega-lite/examples/bar.html *)

(* To run:
   cd examples
   ocamlbuild -use-ocamlfind -package vega-lite,ppx_deriving_yojson bar.byte --
*)

open VegaLite.V2

(*
  Create a row type and equip it with a to-yojson function so it can be used as
  inline data.
*)
type row = {
  a : string;
  b : int
} [@@deriving yojson]

(* Inline data must be a list of Yojson.Safe.json objects. *)
let dataValues = [
    {a = "A"; b = 28}; {a = "B"; b = 55}; {a = "C"; b = 43};
    {a = "D"; b = 91}; {a = "E"; b = 81}; {a = "F"; b = 53};
    {a = "G"; b = 19}; {a = "H"; b = 87}; {a = "I"; b = 52}
  ]

(* Wrap the JSON data in a vega-lite Data object *)
let data = `Inline (InlineData.make ~values:(`Jsons (List.map row_to_yojson dataValues)) ())

(* Create a VegaLite encoding for a bar chart. *)
let encoding =
  let xf = PositionFieldDef.make ~field:(`String "a") ~typ:`Ordinal () in
  let yf = PositionFieldDef.make ~field:(`String "b") ~typ:`Quantitative () in
  EncodingWithFacet.make ~x:(`Field xf) ~y:(`Field yf) ()

(*
  Actually create the spec. We'll accept the defaults for most fields, so they'll
  be 'None' in the OCaml object and will be omitted from the JSON. This spec will
  include the data and the encoding that we created above.
*)
let description = "A simple bar chart with embedded data."
let jsonSpec =
  let open TopLevelFacetedUnitSpec in
  make ~data ~encoding ~mark:(`Mark `Bar) ~description ()
  |> to_yojson


(* Uncomment to print the JSON spec to stdout. *)
(* let () = jsonSpec |> Yojson.Safe.pretty_to_string |> print_endline *)

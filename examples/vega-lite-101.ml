(* To run:
   ocamlbuild -use-ocamlfind -package vega-lite,ppx_deriving_yojson vega-lite-101.byte --
*)

(*
Create a row type and equip it with a to-yojson function so it can be used as
inline data.
*)
type row = {
  x : float;
  y : float
} [@@deriving yojson]

let () =
  let open VegaLite.V2 in
  (* Inline data must be a list of Yojson.Safe.json objects. *)
  let dataValues = List.map row_to_yojson [
      {x=0.0; y=1.0};
      {x=1.0; y=0.0};
      {x=2.0; y=1.0}
    ]
  in
  (* Wrap the JSON data in a vega-lite Data object *)
  let data = `InlineData InlineData.{
      values = `JSON_List dataValues;
      format = None
    }
  in
  (*
  TopLevelFacetedUnitSpec, the top-level single-view plot spec type, requires an
  encoding and a mark. Marks are usually simple variants, but encodings are
  represented by a relatively featureful record type.
  *)
  let encoding : EncodingWithFacet.t =
    let xfield = PositionFieldDef.make ~field:(Some (`String "x")) `Ordinal in
    let yfield = PositionFieldDef.make ~field:(Some (`String "y")) `Ordinal in
    EncodingWithFacet.make
      ~x:(Some (`PositionFieldDef xfield))
      ~y:(Some (`PositionFieldDef yfield))
      ()
  in
  (*
  Actually create the spec. We'll accept the defaults for most fields, so they'll
  be 'None' in the OCaml object and will be omitted from the JSON. This spec will
  include the data and the encoding that we created above.
  *)
  let spec = TopLevelFacetedUnitSpec.make
      ~name:(Some "HelloWorld")
      ~title:(Some (`String "Example Plot"))
      ~description:(Some "An example plot to show you how to use this package")
      ~data:(Some data)
      ~width:(Some 640)
      ~height:(Some 480)
      (`Mark `Line) encoding
  in
  (*
  Transform the spec to JSON, transform the JSON to a string and print the string
  on stdout.
  *)
  spec |> TopLevelFacetedUnitSpec.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline

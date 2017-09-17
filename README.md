# OCaml package 'vega-lite'

This package contains generated OCaml types from the Vega-Lite v2
[spec](https://vega.github.io/schema/vega-lite/v2.json). It is intended to be
typesafe, complete and light on dependencies.

Status: **pre-release**

## Usage

The vega-lite-101.ml example shows you how to create a plot spec in OCaml, then
dump it to Yojson and thence to a string. Here's the code:

```ocaml
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
```


You can run it like this:

```bash
# opam install ocamlbuild vega-lite ppx_deriving_yojson
cd examples
ocamlbuild -use-ocamlfind -package vega-lite,ppx_deriving_yojson vega-lite-101.byte --
```

You'll see JSON output like this:

```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
  "data": {
    "values": [
      { "x": 0.0, "y": 1.0 },
      { "x": 1.0, "y": 0.0 },
      { "x": 2.0, "y": 1.0 }
    ]
  },
  "description": "An example plot to show you how to use this package",
  "encoding": {
    "x": { "field": "x", "type": "ordinal" },
    "y": { "field": "y", "type": "ordinal" }
  },
  "height": 480,
  "mark": "line",
  "name": "HelloWorld",
  "title": "Example Plot",
  "width": 640
}
```

Paste it into the [online Vega editor](https://vega.github.io/editor/#/) and you should
see a simple plot like [this](https://vega.github.io/editor/#/gist/vega-lite/apatil/aeb0a6fdc792aa87ea20c16dcd32f13e).

## Contributing

To re-generate the code in vegaLite.ml, cd to `gen` and run `gen.sh`. You'll need
to install ocamlbild, ppx_tools, ppx_let and yojson first.

To keep the generated code in sync with your repo, you might want to set
`.git/hooks/pre-commit` to

```bash
#!/bin/bash

cd gen
./gen.sh
```

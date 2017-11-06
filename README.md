# OCaml package 'vega-lite'

This package contains generated OCaml types from the Vega-Lite v2
[spec](https://vega.github.io/schema/vega-lite/v2.json). It is intended to be
complete and safe, in the sense that any representable value converts to a valid
Vega-Lite object.

Status: **pre-release**, expect breaking changes.

## Usage

The bar.ml example shows you how to create a plot spec in OCaml, then
dump it to Yojson and thence to a string. Here's the code:

```ocaml
(* https://vega.github.io/vega-lite/examples/bar.html *)

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
let dat = `Inline InlineData.{
    values = `Jsons (List.map row_to_yojson dataValues);
    format = None
  }

(* Create a VegaLite encoding for a bar chart. *)
let enc =
  let xf = PositionFieldDef.(make `Ordinal |> field (`String "a")) in
  let yf = PositionFieldDef.(make `Quantitative |> field (`String "b")) in
  EncodingWithFacet.(make () |> x (`Field xf) |> y (`Field yf))

(*
  Actually create the spec. We'll accept the defaults for most fields, so they'll
  be 'None' in the OCaml object and will be omitted from the JSON. This spec will
  include the data and the encoding that we created above.
*)
let jsonSpec = TopLevelFacetedUnitSpec.(make (`Mark `Bar)
  |> description "A simple bar chart with embedded data."
  |> data dat
  |> encoding enc
  |> to_yojson)

let () = jsonSpec |> Yojson.Safe.pretty_to_string |> print_endline
```


You can run it like this:

```bash
# opam install ocamlbuild vega-lite ppx_deriving_yojson
cd examples
ocamlbuild -use-ocamlfind -package vega-lite,ppx_deriving_yojson bar.byte --
```

You'll see JSON output like this:

```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
  "description": "A simple bar chart with embedded data.",
  "data": {
    "values": [
      {"a": "A","b": 28}, {"a": "B","b": 55}, {"a": "C","b": 43},
      {"a": "D","b": 91}, {"a": "E","b": 81}, {"a": "F","b": 53},
      {"a": "G","b": 19}, {"a": "H","b": 87}, {"a": "I","b": 52}
    ]
  },
  "mark": "bar",
  "encoding": {
    "x": {"field": "a", "type": "ordinal"},
    "y": {"field": "b", "type": "quantitative"}
  }
}
```

Paste it into the [online Vega editor](https://vega.github.io/editor/#/) and you should
see a simple plot like [this](https://vega.github.io/vega-lite/examples/bar.html).

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

## Derivers

If you want the Vega-Lite types to use particular [derivers](https://github.com/ocaml-ppx/ppx_deriving),
you can generate your own version of this module with eg

```bash
cd gen
./gen.sh lens fields
```

This will overwrite `./vegaLite.ml` with a version in which every record type
derives [lens](https://github.com/janestreet/ppx_fields_conv) and
[fields](https://github.com/janestreet/ppx_fields_conv).

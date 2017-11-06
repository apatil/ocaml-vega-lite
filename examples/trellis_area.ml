(* https://vega.github.io/vega-lite/examples/trellis_area.html *)

open VegaLite.V2

let dat = `Url (UrlData.make "data/stocks.csv")

let tf = [
  `Filter (`Filter (`String "datum.symbol !== 'GOOG'"))
]

let enc =
  let xax = Axis.(make ()
    |> title "Time"
    |> format "%Y"
    |> grid false)
  in
  let yax = Axis.(make ()
    |> title "Price"
    |> grid false)
  in

  let xf = PositionFieldDef.(make `Temporal
    |> field  (`String "date")
    |> axis xax)
  in

  let yf = PositionFieldDef.(make `Quantitative
    |> field  (`String "price")
    |> axis yax)
  in

  let col = ConditionalLegendFieldDef.(make `Nominal |> field (`String "symbol")) in

  let r = FacetFieldDef.(make `Nominal
    |> field (`String "symbol")
    |> header Header.(make () |> title "Symbol"))
  in

  EncodingWithFacet.(make ()
    |> x (`Field xf)
    |> y (`Field yf)
    |> color (`Field col)
    |> row r)

let jsonSpec = TopLevelFacetedUnitSpec.(make (`Mark `Area)
  |> description  "Stock prices of four large companies as a small multiples of area charts."
  |> transform tf
  |> data dat
  |> width (`Int 300)
  |> height (`Int 40)
  |> encoding enc
  |> to_yojson)

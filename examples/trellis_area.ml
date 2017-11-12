(* https://vega.github.io/vega-lite/examples/trellis_area.html *)

open VegaLite.V2

let data = `Url (UrlData.make ~url:"data/stocks.csv" ())

let transform = [
  `Filter (`Filter (`String "datum.symbol !== 'GOOG'"))
]

let encoding =
  let xax = Axis.make
      ~title:"Time"
      ~format:"%Y"
      ~grid:false
      ()
  in
  let yax = Axis.make
      ~title:"Price"
      ~grid:false
      ()
  in

  let xf = PositionFieldDef.make
      ~typ:`Temporal
      ~field:(`String "date")
      ~axis:xax
      ()
  in

  let yf = PositionFieldDef.make
      ~typ:`Quantitative
      ~field:(`String "price")
      ~axis:yax
      ()
  in

  let col = ConditionalLegendFieldDef.make ~typ:`Nominal ~field:(`String "symbol") () in

  let r = FacetFieldDef.make
      ~typ:`Nominal
      ~field:(`String "symbol")
      ~header:(Header.make ~title:"Symbol" ())
      ()
  in

  EncodingWithFacet.make
    ~x:(`Field xf)
    ~y:(`Field yf)
    ~color:(`Field col)
    ~row:r
    ()

let jsonSpec = TopLevelFacetedUnitSpec.(
    make
      ~mark:(`Mark `Area)
      ~description:"Stock prices of four large companies as a small multiples of area charts."
      ~transform
      ~data
      ~width:(`Int 300)
      ~height:(`Int 40)
      ~encoding
      ()
    |>to_yojson)

(* https://vega.github.io/vega-lite/examples/stacked_bar_weather.html *)

open VegaLite.V2

let data = `Url (UrlData.make ~url:"data/seattle-weather.csv" ())

let encoding =
  let xax = Axis.make ~title:"Month of the year" () in

  let xf = PositionFieldDef.make
      ~typ:`Ordinal
      ~timeUnit:(`Single (`Local `Month))
      ~field:(`String "date")
      ~axis:xax
      ()
  in

  let legend = Legend.make ~title:"Weather type" () in

  let scale = Scale.make
    ~domain:(`Strings ["sun"; "fog"; "drizzle"; "rain"; "snow"])
    ~range:(`Strings ["#e7ba52";"#c7c7c7";"#aec7e8";"#1f77b4";"#9467bd"])
    ()
  in

  let col = ConditionalLegendFieldDef.make
      ~typ:`Nominal
      ~field:(`String "weather")
      ~legend
      ~scale
      ()
  in

  let yf = PositionFieldDef.make ~typ:`Quantitative ~aggregate:`Count () in

  EncodingWithFacet.make ~color:(`Field col) ~x:(`Field xf) ~y:(`Field yf)  ()

let jsonSpec = TopLevelFacetedUnitSpec.(
    make ~mark:(`Mark `Bar) ~data ~encoding ()
  |> to_yojson)

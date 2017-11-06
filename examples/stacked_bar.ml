(* https://vega.github.io/vega-lite/examples/stacked_bar_weather.html *)

open VegaLite.V2

let dat = `Url (UrlData.make "data/seattle-weather.csv")

let enc =
  let xax = Axis.(make () |> title "Month of the year") in

  let xf = PositionFieldDef.(make `Ordinal
    |> timeUnit (`Single (`Local `Month))
    |> field  (`String "date")
    |> axis xax)
  in

  let leg = Legend.(make () |> title "Weather type") in

  let sc = Scale.(make ()
    |> domain (`Strings ["sun"; "fog"; "drizzle"; "rain"; "snow"])
    |> range (`Strings ["#e7ba52";"#c7c7c7";"#aec7e8";"#1f77b4";"#9467bd"])) in

  let col = ConditionalLegendFieldDef.(make `Nominal
    |> field (`String "weather")
    |> legend leg
    |> scale sc)
  in

  let yf = PositionFieldDef.(make `Quantitative |> aggregate `Count) in

  EncodingWithFacet.(make ()
    |> x (`Field xf)
    |> y (`Field yf)
    |> color (`Field col))

let jsonSpec = TopLevelFacetedUnitSpec.(make (`Mark `Bar)
  |> data dat
  |> encoding enc
  |> to_yojson)

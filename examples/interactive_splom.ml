(* https://vega.github.io/vega-lite/examples/interactive_splom.html *)

open VegaLite.V2

let rep = Repeat.(make ()
  |> column ["Miles_per_Gallon"; "Acceleration"; "Horsepower"]
  |> row ["Horsepower"; "Acceleration"; "Miles_per_Gallon"])

let xf = PositionFieldDef.(make `Quantitative |> field (`Repeat "column"))

let yf = PositionFieldDef.(make `Quantitative |> field (`Repeat "row"))

let colCond = ConditionLegendFieldDef.(make `Nominal (`String "brush")
  |> field (`String "Origin"))

let col = ConditionalLegendValueDef.(make ()
  |> value (`String "grey")
  |> condition (`Field colCond))

let enc = Encoding.(make ()
  |> x (`Field xf)
  |> y (`Field yf)
  |> color (`Value col))

let brushSel = IntervalSelection.(make "interval"
  |> resolve `Union
  |> on (`String "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
  |> translate (`String "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
  |> zoom (`String "wheel![event.shiftKey]"))

let gridSel = IntervalSelection.(make "interval"
  |> resolve `Global
  |> bind "scales"
  |> translate (`String "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!")
  |> zoom (`String "wheel![!event.shiftKey]"))

let sel = [
  ("brush", (`Interval brushSel));
  ("grid", (`Interval gridSel))
]

let sp = CompositeUnitSpec.(make (`Mark `Point)
  |> data (`Url UrlData.(make "data/cars.json"))
  |> selection sel
  |> encoding enc)

let jsonSpec = TopLevelRepeatSpec.(make (`Unit sp)
  |> repeat rep
  |> to_yojson)

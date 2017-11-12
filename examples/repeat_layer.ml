(* https://vega.github.io/vega-lite/examples/repeat_layer.html *)

open VegaLite.V2

let xf = PositionFieldDef.make
    ~typ:`Ordinal
    ~timeUnit:(`Single (`Local `Month))
    ~field:(`String "date")
    ()

let yf = PositionFieldDef.make
    ~typ:`Quantitative
    ~aggregate:`Mean
    ~field:(`Repeat `Column)
    ()

let enc1 =
  let df = FieldDef.make
      ~typ:`Temporal
      ~timeUnit:(`Single (`Local `Year))
      ~field:(`String "date")
      ()
  in
  let col = ConditionalLegendFieldDef.make
      ~typ:`Nominal
      ~field:(`String "location")
      ()
  in
  let op = ConditionalLegendValueDef.make ~value:(`Float 0.2) () in
  Encoding.make
    ~x:(`Field xf)
    ~y:(`Field yf)
    ~detail:(`Field df)
    ~color:(`Field col)
    ~opacity:(`Value op)
    ()

let layer1 = CompositeUnitSpec.make ~mark:(`Mark `Line) ~encoding:enc1 ()

let enc2 =
  let col = ConditionalLegendFieldDef.make ~typ:`Nominal ~field:(`String "location") () in
  Encoding.make
    ~x:(`Field xf)
    ~y:(`Field yf)
    ~color:(`Field col)
    ()

let layer2 = CompositeUnitSpec.make ~mark:(`Mark `Line) ~encoding:enc2 ()

let layerSpec = LayerSpec.make ~layer:[
    (`Unit layer1);
    (`Unit layer2)
  ] ()

let jsonSpec = TopLevelRepeatSpec.(make
    ~spec:(`Layer layerSpec)
    ~description:"Summarized and per year weather information for Seatle and New York."
    ~data:(`Url (UrlData.make ~url:"data/weather.csv" ()))
    ~repeat:(Repeat.make ~column:["temp_max"; "precipitation"; "wind"] ())
    ()
  |> to_yojson)

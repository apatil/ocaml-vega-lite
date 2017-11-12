(* https://vega.github.io/vega-lite/examples/interactive_splom.html *)

open VegaLite.V2

let repeat = Repeat.make
  ~column:["Miles_per_Gallon"; "Acceleration"; "Horsepower"]
  ~row:["Horsepower"; "Acceleration"; "Miles_per_Gallon"]
  ()

let xf = PositionFieldDef.make
    ~field:(`Repeat "column")
    ~typ:`Quantitative
    ()

let yf = PositionFieldDef.make
    ~typ:`Quantitative
    ~field:(`Repeat "row")
    ()

let colCond = ConditionLegendFieldDef.make
    ~typ:`Nominal
    ~selection:(`String "brush")
    ~field:(`String "Origin")
    ()

let col = ConditionalLegendValueDef.make
  ~value:(`String "grey")
  ~condition:(`Field colCond)
  ()

let encoding = Encoding.make
  ~x:(`Field xf)
  ~y:(`Field yf)
  ~color:(`Value col)
  ()

let brushSel = IntervalSelection.make
    ~typ:"interval"
    ~resolve:`Union
    ~on:(`String "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
    ~translate:(`String "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
    ~zoom:(`String "wheel![event.shiftKey]")
    ()

let gridSel = IntervalSelection.make
    ~typ:"interval"
    ~resolve:`Global
    ~bind:"scales"
    ~translate:(`String "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!")
    ~zoom:(`String "wheel![!event.shiftKey]")
    ()

let selection = [
  ("brush", (`Interval brushSel));
  ("grid", (`Interval gridSel))
]

let sp = CompositeUnitSpec.make
    ~mark:(`Mark `Point)
    ~data:(`Url (UrlData.make ~url:"data/cars.json" ()))
    ~selection
    ~encoding
    ()

let jsonSpec = TopLevelRepeatSpec.(make
                                     ~spec:(`Unit sp)
                                     ~repeat
                                     ()
                                  |> to_yojson)

(* {
  "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
             "description": "Stock prices of 5 Tech Companies over Time with Averages.",
             "data": {"url": "data/stocks.csv"},
             "width": 640,
              "height": 480,
             "layer": [
               {
                 "mark": "line",
                         "encoding": {
                           "x": {"field": "date", "type": "temporal"},
                                "y": {"field": "price", "type": "quantitative"},
                                "color": {"field": "symbol", "type": "nominal"}
                         }
               },
               {
                 "mark": "rule",
                         "encoding": {
                           "y": {
                             "field": "price",
                                      "type": "quantitative",
                                      "aggregate": "mean"
                           },
                             "size": {"value": 2},
                             "color": {"field": "symbol", "type": "nominal"}
                         }
               }
             ]
   }

   stocks.csv looks like this:

symbol,date,price
MSFT,Jan 1 2000,39.81
MSFT,Feb 1 2000,36.35
MSFT,Mar 1 2000,43.22
MSFT,Apr 1 2000,28.37

*)

module Vega = struct
  module Data = struct
    type t = {url: string} [@@deriving yojson] (* Can be richer *)
  end

  module Mark = struct
    type markType =
      | `bar
      | `circle
      | `square
      | `tick
      | `line
      | `area
      | `point
      | `rule
      | `text
    [@@deriving yojson]

    type t = {
      markType: markType [@key "type"];
      style: string list;
      clip: bool;
      filled: bool
    } [@@deriving yojson]

    let default: t = {
      typ = `line;
      style = [];
      clip = false;
      fill = false
    }
  end

  module Bin = struct
    type params = {
      maxBins: int;
      base: int;
      step: int option;
      steps: int list;
      minstep: int option;
      divide: int list;
      extent: (int * int) option;
      nice: bool
    } [@@deriving yojson]

    let default_params : params = {
      maxBins = 6;
      base = 10;
      step = None;
      steps = [];
      minstep = None;
      divide = [5, 2];
      extent = None;
      nice = true
    }

    type t =
      | `Bool of bool
      | `BinParams of binParams
    let of_yojson = function
      | `Bool b -> `Bool b
      | `BinParams bp -> yojson_of_binParams bp
    let bin_of_yojson = function
      | `Bool b -> `Bool b
      | x -> binParams_of_yojson x

  type timeUnit =
    | `year
    | `yearquarter
    | `yearquartermonth
    | `yearmonth
    | `yearmonthdate
    | `yearmonthdatehours
    | `yearmonthdatehoursminutes
    | `yearmonthdatehoursseconds
    | `quarter
    | `quartermonth
    | `month
    | `monthdate
    | `date
    | `day
    | `hours
    | `hoursminutes
    | `hoursminutesseconds
    | `minutes
    | `minutesseconds
    | `seconds
    | `secondsmilliseconds
    | `milliseconds
  [@@deriving yojson]

  type encodingType =
    | `quantitative
    | `temporal
    | `ordinal
    | `nominal
  [@@deriving yojson]

  type encodingChannel =
    | `x
    | `y
    | `x2
    | `y2` (* And much much more *)
  [@@deriving yojson]

  type encoding = {
    field: encodingChannel; (* Note, can be richer *)
    typ: encodingType [@key "type"];
    bin: bin option;
    timeUnit: timeUnit option
    (* aggregate,scale, axis, legend, format, stack, sort, condition *)
  } [@@deriving yojson]
  let dflt_encoding : encoding = {

  }

  type layer = {
    mark: string;
    encoding: (encodingChannel * encoding) Map
  } [@@deriving yojson]

  type t = {
    (* $schema is https://vega.github.io/schema/vega-lite/v2.json *)
    description: string;
    data: data;
    width: int option;
    height: int option;
    layer: layer list
  } [@@deriving yojson]
end

(* vega html *)
(* <html>
<head>
  <title>Embedding Vega-Lite</title>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/vega/3.0.1/vega.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/vega-lite/2.0.0-beta.19/vega-lite.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/vega-embed/3.0.0-beta.20/vega-embed.js"></script>
</head>
<body>

  <div id="vis"></div>

  <script type="text/javascript">
    var yourVlSpec = {
      "$schema": "https://vega.github.io/schema/vega-lite/v2.0.json",
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
    vega.embed("#vis", yourVlSpec);
  </script>
</body>
</html> *)

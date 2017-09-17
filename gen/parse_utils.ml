open Let_syntax

let untilSuccess (msg : string) (vals : ('a, string) result list): ('a, string) result =
  let indent s = (String.split_on_char '\n' s) |> (List.map (fun ss -> "    " ^ ss)) |> (String.concat "\n") in
  let reducer (sofar : ('a, string) result) (next : ('a, string) result) : ('a, string) result =
    match sofar with
    | Ok x -> Ok x
    | Error m -> (match next with
        | Ok x -> Ok x
        | Error mnew -> Error (m ^ "\n  - " ^ (indent mnew)))
  in
  List.fold_left reducer (Error msg) vals

let prependError (msg : string) (x : ('a, string) result) : ('a, string) result =
  match x with
  | Error s -> Error (msg ^ s)
  | o -> o

(* Some utilities for string maps *)
module StringMap = Map.Make(String)
let mapGet (k : string) (m :'a StringMap.t) : ('a, string) result =
  try
    Ok (StringMap.find k m)
  with _ -> Error ("mapGet: Key '" ^ k ^ "' not found")

let assertStringEqual (s1 : string) (s2 : string) : (unit, string) result =
  match String.equal s1 s2 with
  | true -> Ok ()
  | false -> Error "assertStringEqual: Not equal"

let opt_of_res (res : ('a, 'b) result) : 'a option =
  match res with
  | Ok x -> Some x
  | _ -> None

module TopoSort = struct
  open Let_syntax
  type node = (string * string list)
  type graph = node list
  exception CycleFound
  let dfs (graph : graph) (visited : string list) (start_node : node) : (string list, string) result =
    let rec explore (path : string list) (mv : (string list, string) result) ((name, parents) : node) : (string list, string) result =
      let%bind visited_ = mv in
      if List.exists (String.equal name) path then Error ("cycle found involving " ^ name) else
      if List.exists (String.equal name) visited_ then Ok visited_ else
        let new_path = name :: path in
        let%bind pnodes = try
          Ok (List.map (fun p -> (p, List.assoc p graph)) parents)
        with
        | _ -> Error "parent not found in graph"
        in
        let%bind visited_  = List.fold_left (explore new_path) (Ok visited_) pnodes in
        Ok (name :: visited_)
    in explore [] (Ok visited) start_node

  let toposort (graph : graph) : (string list, string) result =
    List.fold_left (fun mv node -> mv >>= fun visited -> dfs graph visited node) (Ok []) graph
end

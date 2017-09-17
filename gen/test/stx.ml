module type Blarg = sig
  type t = x
  val blarg : ?x:(int option) -> unit -> int option
end

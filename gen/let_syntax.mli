val return : 'a -> ('a, 'e) result
val bind   : ('a, 'e) result -> f:('a -> ('b, 'e) result) -> ('b, 'e) result
val map    : ('a, 'e) result -> f:('a -> 'b) -> ('b, 'e) result
val both   : ('a, 'e) result -> ('b, 'e) result -> (('a * 'b), 'e) result
val (>>=) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val (>>|) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

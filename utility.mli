exception EOF

val pi : float

val square : float -> float

val empty : 'a list -> bool

val head_or : 'a -> 'a list -> 'a

val join : 
  conversion:('a -> string) -> list:'a list -> ?separator:string -> unit -> string

val map_or_not :
  f:('a -> 'b) -> list:'a list -> 'b list

val print_bool : bool -> unit

val flat_map : f:('a -> 'b list) -> list:'a list -> 'b list

val checked_asin : float -> float

val checked_acos : float -> float

val extract_last : 'a list -> 'a list * 'a

val option_get : 'a option -> 'a

val sgn : float -> float

val input : in_channel -> int -> string

val input_until_eof : in_channel -> string

class [ 'parameter ] signal : object
  method connect : ('parameter -> unit) -> int
  method disconnect : int -> unit
  method call : 'parameter -> unit
end

val argmin : badness:('a -> 'b) -> 'a list -> 'a

val list_first_n : n:int -> 'a list -> 'a list

val list_past_n : n:int -> 'a list -> 'a list

val collect : (unit -> 'a) -> 'a list

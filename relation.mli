open Bigarray

exception MaxElementsMismatch

class transitive_relation : int -> object
  method matrix : unit -> ( int,Bigarray.int_elt,Bigarray.c_layout ) Array2.t
  method max_elements : unit -> int 
  method copy_from : transitive_relation -> unit 
  method is : int -> int -> bool
  method add : int -> int -> unit 
  method is_irreflexive : unit -> bool
end

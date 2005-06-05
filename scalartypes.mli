(* these types are examples of Genhomogeneous.SCALAR_TYPE *)

module FloatScalars : sig
  type t = float

  val neutral_add : t
  val neutral_multiply : t
  val add : t -> t -> t
  val multiply : t -> t -> t
  val negate : t -> t
  val to_string : t -> string

  val sin : t -> t
  val cos : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan2 : t -> t -> t
  val power : t -> float -> t
end

module SymbolicScalars : sig
  type t = Symbolic.term
  
  val neutral_add : t
  val neutral_multiply : t
  val add : t -> t -> t
  val multiply : t -> t -> t
  val negate : t -> t
  val to_string : t -> string

  val sin : t -> t
  val cos : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan2 : t -> t -> t
  val power : t -> float -> t
end


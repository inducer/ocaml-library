(* this is supposed to be a superset of Genmatrix.SCALAR_TYPE *)
module type SCALAR_TYPE = sig
  type t

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


module Homogeneous : functor ( Scalar : SCALAR_TYPE) -> sig

  type scalar = Scalar.t
  type matrix = Genmatrix.Matrix(Scalar).t
  
  (* creation -------------------------------------------------------------------
   *)
  val translation : scalar list -> matrix
  val rotation : dimension:int -> plane_index:int -> plane_index_2:int -> scalar -> matrix
  val rotation_2d : scalar -> matrix
  val rotation_x : scalar -> matrix
  val rotation_y : scalar -> matrix
  val rotation_z : scalar -> matrix
  val rotation_all : scalar -> scalar -> scalar -> matrix

  (* analysis -------------------------------------------------------------------
   *)
  val get_rotation_x : matrix -> scalar
  val get_rotation_y : matrix -> scalar
  val get_rotation_z : matrix -> scalar
  val get_rotation_2d : matrix -> scalar

  val rotation_matrix : matrix -> matrix
  val location_vector : matrix -> matrix

  (* operations -----------------------------------------------------------------
   *)
  val invert : matrix -> matrix
end

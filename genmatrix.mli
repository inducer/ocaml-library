module type SCALAR_TYPE = sig
  type t
  val neutral_add : t
  val neutral_multiply : t
  val add : t -> t -> t
  val multiply : t -> t -> t
  val negate : t -> t
  val power : t -> float -> t
  val to_string : t -> string
end


module Matrix : functor ( Scalar : SCALAR_TYPE) -> sig
  type scalar = Scalar.t
  type t

  (* information --------------------------------------------------------------
   *)
  val height : t -> int
  val width : t -> int
  val get : t -> int -> int -> scalar
  val set : t -> int -> int -> scalar -> unit

  (* creation -----------------------------------------------------------------
   *)
  val create : int -> int -> t
  val create_zero : int -> int -> t 
  val create_identity : int -> t
  val assign : t -> t -> t
  val assign_zero : t -> t
  val copy : t -> t
  val restrict  : t -> i:int -> j:int -> height:int -> width:int -> t 
  val restrict_row : t -> int -> t
  val restrict_column : t -> int -> t
  val submatrix : t -> i:int -> j:int -> height:int -> width:int -> t 
  val copy_row : t -> int -> t
  val copy_column : t -> int -> t
  val of_array : scalar array array -> t
  val column_vector_of_array : scalar array -> t
  val row_vector_of_array : scalar array -> t
  val column_vector_of_list : scalar list -> t
  val row_vector_of_list : scalar list -> t

  (* arithmetic ---------------------------------------------------------------
   *)
  val multiply : t -> t -> t
  val multiply_scalar : t -> scalar -> t 
  val multiply_scalar_inplace : t -> scalar -> t
  val add : t -> t -> t
  val add_inplace: t -> t -> t
  val negate : t -> t
  val negate_inplace : t -> t
  val subtract : t -> t -> t
  val subtract_inplace : t -> t -> t

  (* other operations ---------------------------------------------------------
   *)
  val map : (int -> int -> scalar -> scalar) -> t -> t
  val transpose : t -> t
  val transpose_inplace : t -> t
  val trace : t -> scalar
  val euclidean_norm_square : t -> scalar
  val is_zero : t -> bool

  (* i/o ----------------------------------------------------------------------
   *)
  val print : t -> unit
end

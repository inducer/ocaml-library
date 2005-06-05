exception NoPivot
exception NoSolution
exception MatrixSingular




type scalar = float
type t




(* information ----------------------------------------------------------------
*)
val height : t -> int
val width : t -> int
val get : t -> int -> int -> scalar
val set : t -> int -> int -> scalar -> unit

(* creation -------------------------------------------------------------------
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
val list_of_column_vector : t -> scalar list
val list_of_row_vector : t -> scalar list

(* arithmetic -----------------------------------------------------------------
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

(* other operations -----------------------------------------------------------
*)
val transpose : t -> t
val transpose_inplace : t -> t
val trace : t -> scalar
val euclidean_norm : t -> scalar
val is_zero : t -> bool

(* gauss ----------------------------------------------------------------------
*)
val gauss_jordan_inplace : 
  t -> columns:int -> t * int list (* list of nonzero columns *)
val linear_solve_subspace : 
  t -> t -> t * t
  (* inhomogeneous solution, matrix of homogeneous solutions *)
val linear_solve : t -> t -> t
val invert : t -> t

(* i/o ------------------------------------------------------------------------
*)
val read : in_channel -> t
val print : t -> unit
val list_of_matrix : t -> scalar list

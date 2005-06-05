(* creation -------------------------------------------------------------------
*)
val translation : Matrix.scalar list -> Matrix.t
val rotation : dimension:int -> plane_index:int -> plane_index_2:int -> float -> Matrix.t
val rotation_2d : float -> Matrix.t
val rotation_x : float -> Matrix.t
val rotation_y : float -> Matrix.t
val rotation_z : float -> Matrix.t
val rotation_all : float -> float -> float -> Matrix.t

(* analysis -------------------------------------------------------------------
*)
val get_rotation_x : Matrix.t -> float
val get_rotation_y : Matrix.t -> float
val get_rotation_z : Matrix.t -> float
val get_rotation_2d : Matrix.t -> float

val rotation_matrix : Matrix.t -> Matrix.t
val location_vector : Matrix.t -> Matrix.t

(* operations -----------------------------------------------------------------
*)
val invert : Matrix.t -> Matrix.t

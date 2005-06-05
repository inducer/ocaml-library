open Utility



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


module Homogeneous = functor ( Scalar : SCALAR_TYPE) -> struct
  module Matrix = Genmatrix.Matrix(Scalar)

  type scalar = Scalar.t
  type matrix = Matrix.t
  
  let sqrt x = Scalar.power x 0.5
  let square x = Scalar.power x 2.
  
  (* creation -----------------------------------------------------------------
   *)
  let translation dim_list =
    let
      dimension = List.length dim_list
    in
    let
      result = Matrix.create_identity (dimension + 1)
    in
    let rec make_trans dim_list i =
      match dim_list with
        | x :: tail -> 
            Matrix.set result i dimension x;
	    make_trans tail (i+1)
        | [] -> ()
    in
      make_trans dim_list 0;
      result




  let rotation ~dimension ~plane_index ~plane_index_2 alpha =
    let 
      result = Matrix.create_identity (dimension+1)
    in
      Matrix.set result plane_index plane_index (Scalar.cos alpha);
      Matrix.set result plane_index_2 plane_index_2 (Scalar.cos alpha);
      Matrix.set result plane_index_2 plane_index (Scalar.sin alpha);
      Matrix.set result plane_index plane_index_2 (Scalar.negate (Scalar.sin alpha));
      result

  let rotation_2d = rotation ~dimension:2 ~plane_index:0 ~plane_index_2:1
  let rotation_x = rotation ~dimension:3 ~plane_index:1 ~plane_index_2:2 
  let rotation_y = rotation ~dimension:3 ~plane_index:2 ~plane_index_2:0 
  let rotation_z = rotation ~dimension:3 ~plane_index:0 ~plane_index_2:1 

  let rotation_all x y z =
    Matrix.multiply (rotation_x x)
      (Matrix.multiply (rotation_y y)
         (rotation_z z))




  (* analysis -----------------------------------------------------------------
   *)
  let get_rotation_x matrix =
    Scalar.atan2 (Matrix.get matrix 2 1) (Matrix.get matrix 2 2)

  let get_rotation_y matrix =
    Scalar.atan2 
      (Scalar.negate 
         (Matrix.get matrix 2 0)) 
      (sqrt (Scalar.add (square (Matrix.get matrix 0 0)) (square (Matrix.get matrix 1 0))))

  let get_rotation_z matrix =
    Scalar.atan2 (Matrix.get matrix 1 0) (Matrix.get matrix 0 0)

  let get_rotation_2d matrix =
    get_rotation_z matrix
      



  (* submatrices --------------------------------------------------------------
   *)
  let rotation_matrix matrix =
    (Matrix.restrict matrix ~i:0 ~j:0 ~height:((Matrix.height matrix)-1) ~width:((Matrix.width matrix)-1))
    



  let location_vector matrix =
    (Matrix.restrict matrix ~i:0 ~j:((Matrix.width matrix)-1) ~height:((Matrix.height matrix)-1) ~width:1)




  (* operations ---------------------------------------------------------------
   *)
  let invert matrix =
    let 
      result = Matrix.create_identity (Matrix.height matrix)
    in
      (ignore (Matrix.assign 
                 (rotation_matrix result) 
                 (Matrix.transpose (rotation_matrix matrix))));
      (ignore (Matrix.assign 
                 (location_vector result)
                 (Matrix.multiply (rotation_matrix result) (location_vector matrix))));
      result
end

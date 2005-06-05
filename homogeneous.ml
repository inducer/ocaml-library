open Utility
open Matrix




(* creation -------------------------------------------------------------------
*)
let translation dim_list =
  let
    dimension = List.length dim_list
  in
  let
    result = create_identity (dimension + 1)
  in
  let rec make_trans dim_list i =
    match dim_list with
      | x :: tail -> 
          set result i dimension x;
	  make_trans tail (i+1)
      | [] -> ()
  in
    make_trans dim_list 0;
    result




let rotation ~dimension ~plane_index ~plane_index_2 alpha =
  let 
    result = create_identity (dimension+1)
  in
    set result plane_index plane_index (cos alpha);
    set result plane_index_2 plane_index_2 (cos alpha);
    set result plane_index_2 plane_index (sin alpha);
    set result plane_index plane_index_2 (-.(sin alpha));
    result

let rotation_2d = rotation ~dimension:2 ~plane_index:0 ~plane_index_2:1
let rotation_x = rotation ~dimension:3 ~plane_index:1 ~plane_index_2:2 
let rotation_y = rotation ~dimension:3 ~plane_index:2 ~plane_index_2:0 
let rotation_z = rotation ~dimension:3 ~plane_index:0 ~plane_index_2:1 

let rotation_all x y z =
  Matrix.multiply (rotation_x x)
    (Matrix.multiply (rotation_y y)
       (rotation_z z))




(* analysis -------------------------------------------------------------------
*)
let get_rotation_x matrix =
  atan2 (get matrix 2 1) (get matrix 2 2)

let get_rotation_y matrix =
  atan2 (-.(get matrix 2 0)) (sqrt ((square (get matrix 0 0)) +. (square (get matrix 1 0))))

let get_rotation_z matrix =
  atan2 (get matrix 1 0) (get matrix 0 0)

let get_rotation_2d matrix =
  get_rotation_z matrix
  



(* submatrices ----------------------------------------------------------------
*)
let rotation_matrix matrix =
  (restrict matrix ~i:0 ~j:0 ~height:((Matrix.height matrix)-1) ~width:((Matrix.width matrix)-1))
  



let location_vector matrix =
  (restrict matrix ~i:0 ~j:((Matrix.width matrix)-1) ~height:((Matrix.height matrix)-1) ~width:1)




(* operations -----------------------------------------------------------------
*)
let invert matrix =
  let 
    result = create_identity (Matrix.height matrix)
  in
    (ignore (assign 
       (rotation_matrix result) 
       (transpose (rotation_matrix matrix))));
    (ignore (assign 
       (location_vector result)
       (multiply (rotation_matrix result) (location_vector matrix))));
    result


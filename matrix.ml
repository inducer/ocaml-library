open Bigarray




exception NoPivot
exception NoSolution
exception MatrixSingular
exception BoolResult of bool (* internal *)




type restriction_info = {
  start_i : int;
  start_j : int;
  height : int;
  width : int
}

type scalar = float

type bigarray_matrix = (scalar, float64_elt, c_layout) Array2.t

type t = Full of bigarray_matrix 
              | Restricted of restriction_info * bigarray_matrix




(* information ----------------------------------------------------------------
*)
let height matrix =
  match matrix with 
    | Full(matrix) -> Array2.dim1 matrix
    | Restricted(info,_) -> info.height
  



let width matrix =
  match matrix with 
    | Full(matrix) -> Array2.dim2 matrix
    | Restricted(info,_) -> info.width




let get matrix i j =
  match matrix with 
    | Full(matrix) -> 
        Array2.get matrix i j
    | Restricted(info,matrix) -> 
        Array2.get matrix (info.start_i+i) (info.start_j+j)




let set matrix i j value =
  match matrix with 
    | Full(matrix) -> 
        Array2.set matrix i j value
    | Restricted(info,matrix) -> 
        Array2.set matrix (info.start_i+i) (info.start_j+j) value




(* creation -------------------------------------------------------------------
*)
let create height width =
  Full(Array2.create float64 c_layout height width)
  



let create_zero height width =
  let
    result = Array2.create float64 c_layout height width
  in
    Array2.fill result 0.;
    Full(result)
    



let create_identity dim =
  let
    result = create_zero dim dim
  in
    for i = 0 to dim - 1 do
      set result i i 1.
    done;
    result
    



let assign_zero dest =
  for x = 0 to (width dest) - 1 do
    for y = 0 to (height dest) - 1 do 
      (set dest y x 0.)
    done
  done;
  dest




let assign dest src =
  if (width dest) <> (width src) then begin
    raise (Failure "assignment of non-matching matrices")
  end;
  if (height dest) <> (height src) then begin
    raise (Failure "assignment of non-matching matrices")
  end;
  for x = 0 to (width src) - 1 do
    for y = 0 to (height src) - 1 do 
      (set dest y x (get src y x))
    done
  done;
  dest




let copy matrix =
  let 
    result = create (height matrix) (width matrix)
  in
    ignore (assign result matrix);
    result




let restrict matrix ~i ~j ~height ~width =
  match matrix with
    | Full(matrix) ->
        Restricted(
          { start_i = i; start_j = j; height = height; width = width }, 
          matrix
        )
    | Restricted(info,matrix) ->
        Restricted(
          { start_i = info.start_i+i;
            start_j = info.start_j+j; 
            height = height;
            width = width }, 
          matrix
        )




let restrict_row matrix row =
  restrict matrix ~i:row ~j:0 ~height:1 ~width:(width matrix)




let restrict_column matrix column =
  restrict matrix ~i:0 ~j:column ~height:(height matrix) ~width:1
  



let submatrix matrix ~i ~j ~height ~width =
  copy (restrict matrix ~i ~j ~height ~width)
 



let copy_row matrix row =
  copy (restrict_row matrix row)
    
    


let copy_column matrix column =
  copy (restrict_column matrix column)




let of_array array =
  Full(Array2.of_array float64 c_layout array)




let row_vector_of_array array =
  of_array [| array |]




let list_of_column_vector matrix =
  let list = ref []
  in
    for y = (height matrix) - 1 downto 0 do 
      list := (get matrix y 0) :: !list
    done;
    !list
    



let list_of_row_vector matrix =
  let list = ref []
  in
    for x = (width matrix) - 1 downto 0 do 
      list := (get matrix 0 x) :: !list
    done;
    !list
    



(* arithmetic -----------------------------------------------------------------
*)
let multiply mat1 mat2 =
  if (width mat1) <> (height mat2) then begin
    raise (Failure "multiplication of non-matching matrices")
  end;
  let
    result = create (height mat1) (width mat2)
  in
    for x = 0 to (width mat2) - 1 do
      for y = 0 to (height mat1) - 1 do 
        let 
          sum = ref 0.
        in
          for z = 0 to (width mat1) - 1 do
            sum := !sum +. (get mat1 y z) *. (get mat2 z x)
          done;
          set result y x !sum
      done
    done;
    result




let multiply_scalar_inplace mat1 scalar =
  for x = 0 to (width mat1) - 1 do
    for y = 0 to (height mat1) - 1 do
      (set mat1 y x ((get mat1 y x) *. scalar))
    done
  done;
  mat1




let multiply_scalar mat1 scalar =
  let
    result = create (height mat1) (width mat1)
  in
    for x = 0 to (width mat1) - 1 do
      for y = 0 to (height mat1) - 1 do
        (set result y x ((get mat1 y x) *. scalar))
      done
    done;
    result




let add mat1 mat2 =
  if (width mat1) <> (width mat2) then begin
    raise (Failure "addition of non-matching matrices")
  end;
  if (height mat1) <> (height mat2) then begin
    raise (Failure "addition of non-matching matrices")
  end;
  let
    result = create (height mat1) (width mat1)
  in
    for x = 0 to (width mat1) - 1 do
      for y = 0 to (height mat1) - 1 do
        (set result y x ((get mat1 y x) +. (get mat2 y x)))
      done
    done;
    result




let add_inplace mat1 mat2 =
  if (width mat1) <> (width mat2) then begin
    raise (Failure "addition of non-matching matrices")
  end;
  if (height mat1) <> (height mat2) then begin
    raise (Failure "addition of non-matching matrices")
  end;
  for x = 0 to (width mat1) - 1 do
    for y = 0 to (height mat1) - 1 do
      (set mat1 y x ((get mat1 y x) +. (get mat2 y x)))
    done
  done;
  mat1




let negate matrix =
  multiply_scalar matrix (-. 1.)




let negate_inplace matrix =
  multiply_scalar_inplace matrix (-. 1.)
  



let subtract mat1 mat2 =
  add mat1 (negate mat2)




let subtract_inplace mat1 mat2 =
  add_inplace mat1 (negate_inplace mat2)




(* other operations -----------------------------------------------------------
*)
let transpose matrix =
  let
    result = create (width matrix) (height matrix)
  in
    for x = 0 to (width matrix) - 1 do
      for y = 0 to (height matrix) - 1 do
        (set result x y (get matrix y x))
      done
    done;
    result
  




let transpose_inplace matrix =
  if (width matrix) <> (width matrix) then begin
    raise (Failure "inplace-transpose of non-n*n matrix")
  end;
  for x = 0 to (width matrix) - 1 do
    for y = 0 to (height matrix) - 1 do
      let 
        temp = (get matrix x y)
      in
        (set matrix x y (get matrix y x));
        (set matrix y x temp)
    done
  done;
  matrix




let trace matrix = 
  if (width matrix) <> (width matrix) then begin
    raise (Failure "trace of non-n*n matrix")
  end;
  let 
    sum = ref 0.
  in
    for x = 0 to (width matrix) - 1 do
      sum := !sum +. (get matrix x x)
    done;
    !sum
    
    
    
  

let scalar_product vector1 vector2 =
  if (width vector1) <> 1 then begin
    raise (Failure "scalar_product of non-vector")
  end;
  if (width vector2) <> 1 then begin
    raise (Failure "scalar_product of non-vector")
  end;
  get (multiply (transpose vector1) vector2) 0 0




let euclidean_norm vector =
  sqrt (scalar_product vector vector)




let is_zero matrix = 
  try
    for x = 0 to (width matrix) - 1 do
      for y = 0 to (height matrix) - 1 do
        if (get matrix y x) <> 0. then
          raise (BoolResult false)
      done
    done;
    true
  with
      BoolResult(x) -> x
  

(* gauss ----------------------------------------------------------------------
*)
let add_row matrix dest src factor =
  let 
    restrict_dest = restrict_row matrix dest 
  and 
    copy_src = copy_row matrix src 
  in
    add_inplace restrict_dest (multiply_scalar_inplace copy_src factor)




let multiply_row matrix dest factor =
  let 
    restrict_dest = restrict_row matrix dest
  in
    multiply_scalar_inplace restrict_dest factor




let swap_row matrix a b =
  if a <> b then
    let 
      copy_a = copy_row matrix a 
    and 
      restrict_a = restrict_row matrix a 
    and 
      restrict_b = restrict_row matrix b
    in
      ignore (assign restrict_a restrict_b);
      ignore (assign restrict_b copy_a)
        




let search_pivot matrix start_row column =
  let rec search_pivot_internal current_row max_index max_value =
    if current_row = height matrix then 
      max_index
    else
      let 
        current_value = (abs_float (get matrix current_row column))
      in
        if current_value > max_value then
          search_pivot_internal (current_row + 1) current_row current_value
        else
          search_pivot_internal (current_row + 1) max_index max_value
  in
  let 
    result = search_pivot_internal start_row (-1) 0.
  in
    if result = -1 then 
      raise NoPivot
    else
      result




let gauss_jordan_inplace matrix ~columns =
  let current_column = ref 0
  and current_row = ref 0
  and h = height matrix 
  and nonzero_columns = ref []
  in
    while !current_row < h  && !current_column < columns do 
      try
        let 
          pivot = search_pivot matrix !current_row !current_column 
        in
          swap_row matrix !current_row pivot;
          let 
            pivot_value = (get matrix !current_row !current_column)
          in
            for i = !current_row + 1 to h - 1 do
              ignore 
	        (add_row matrix i !current_row 
		   (-.(get matrix i !current_column) /. pivot_value));
              set matrix i !current_column 0.
            done;
            for i = 0 to !current_row - 1 do
              ignore 
	        (add_row matrix i !current_row 
		   (-.(get matrix i !current_column) /. pivot_value));
              set matrix i !current_column 0.
            done;
	    nonzero_columns := !current_column :: !nonzero_columns;
	    current_column := !current_column + 1;
	    current_row := !current_row + 1;
      with
        | NoPivot -> 
	    current_column := !current_column + 1
    done;
    matrix,List.rev !nonzero_columns




  
let linear_solve_subspace matrix vector =
  if height matrix <> height vector then begin
    raise (Failure "linear_solve: heights don't match")
  end;
  let 
    composite = create (height matrix) ((width matrix) + (width vector))
  in
  let
    composite_left = restrict composite ~i:0 ~j:0 ~height:(height matrix) 
                       ~width:(width matrix)
  and
    composite_right = restrict composite ~i:0 ~j:(width matrix) ~height:(height matrix) 
                        ~width:(width vector)
  and h = height matrix
  and w = width matrix
  in
    ignore (assign composite_left matrix);
    ignore (assign composite_right vector);
    let 
      matrix,nonzero_columns = gauss_jordan_inplace composite w
    in
    let rec complement column nonzero_columns =
      if column < w then
        match nonzero_columns with
          | head :: tail ->
	      if head = column then
	        complement (column + 1) tail
	      else
	        column :: complement (column + 1) nonzero_columns
	  | [] ->
	      column :: complement (column + 1) nonzero_columns
      else
        []
    in
    let zero_columns = complement 0 nonzero_columns
    in
    let rec normalize column row zero_columns =
      if column < w && row < h then begin
        match zero_columns with 
	  | head :: tail ->
	      if column = head then
	        (* nothing to normalize here *)
		normalize (column + 1) row tail
	      else
		ignore (multiply_row composite row (1. /. (get composite row column)));
		normalize (column + 1) (row + 1) zero_columns
	  | [] ->
	      ignore (multiply_row composite row (1. /. (get composite row column)));
	      normalize (column + 1) (row + 1) zero_columns
      end
      else
        ()
    in
      normalize 0 0 zero_columns;
      (* dimension count of matrix kernel *)
      let corank = List.length zero_columns
      in
      let first_zero_row = List.length nonzero_columns
      in
        for i = first_zero_row to h - 1 do
          if not (is_zero (restrict_row composite_right i)) then
	    raise NoSolution
        done;
        let kernel_matrix = create_zero w corank
        in
        let solution_vector = create_zero w (width vector)
	in
	let rec fill_matrices row zero_rows nonzero_rows zero_columns =
	  let process_zero_row zero_column tail =
	    (* fill solution vector: zero *)
            
	    (* fill kernel matrix *)
	    set kernel_matrix row zero_rows (-.1.);
	    fill_matrices 
	      (row + 1) 
	      (zero_rows + 1) 
	      nonzero_rows
	      tail
	  and process_nonzero_row () =
	    (* fill solution vector *)
	    ignore (assign 
		      (restrict_row solution_vector row)
		      (restrict_row composite_right nonzero_rows));
	    
	    (* fill kernel matrix *)
	    let column = ref zero_columns
	    in
	      for i = zero_rows to corank - 1 do
		set kernel_matrix row i 
		  (get composite nonzero_rows (List.hd !column));
		column := List.tl !column
	      done;
	      fill_matrices 
		(row + 1) 
		zero_rows 
		(nonzero_rows + 1)
		zero_columns
          in
	    if row < w then
              match zero_columns with
                | zero_column :: tail ->
                    if zero_column = row then 
		      process_zero_row zero_column tail
		    else
		      process_nonzero_row ()
	        | [] ->
		    process_nonzero_row ()
        in
          fill_matrices 0 0 0 zero_columns;
          solution_vector,kernel_matrix




let linear_solve matrix vector =
  fst (linear_solve_subspace matrix vector)




let invert matrix =
  let solution,subspace = linear_solve_subspace matrix (create_identity (width matrix))
  in
    if width subspace <> 0 then
      raise MatrixSingular
    else
      solution

                       
                       

(* i/o ------------------------------------------------------------------------
*)
let read in_channel =
  let 
    height = int_of_string (input_line in_channel)
  and 
    width = int_of_string (input_line in_channel)
  in
  let
    matrix = create height width
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      set matrix y x (float_of_string (input_line in_channel))
    done
  done;
  matrix
  




let print matrix =
  for y = 0 to (height matrix) - 1 do
    for x = 0 to (width matrix) - 1 do
      Printf.printf "%f " (get matrix y x)
    done;
    print_newline ()
  done




let list_of_matrix matrix =
  let 
    list = ref []
  in
    for y = 0 to (height matrix) - 1 do
      for x = 0 to (width matrix) - 1 do
        list := (get matrix y x) :: !list
      done;
    done;
    List.rev !list




(* more creation --------------------------------------------------------------
*)
let column_vector_of_array array =
  transpose (row_vector_of_array array)




let column_vector_of_list list =
  let dim = List.length list
  in
  let result = create dim 1
  in
  let list = ref list
  in
    for i = 0 to dim - 1 do 
      set result i 0 (List.hd !list);
      list := List.tl !list
    done;
    result




let row_vector_of_list list =
  let dim = List.length list
  in
  let result = create 1 dim 
  in
  let list = ref list
  in
    for i = 0 to dim - 1 do 
      set result 0 i (List.hd !list);
      list := List.tl !list
    done;
    result





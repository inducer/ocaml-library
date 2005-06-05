exception BoolResult of bool (* internal *)




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


module Matrix = functor ( Scalar : SCALAR_TYPE) -> struct
  type size_info = {
    start_i : int;
    start_j : int;
    height : int;
    width : int
  }

  type scalar = Scalar.t

  type matrix = scalar array array 

  type t = size_info * matrix




  (* information ----------------------------------------------------------------
   *)
  let height matrix =
    let info,matrix = matrix
    in
      info.height
          



  let width matrix =
    let info,matrix = matrix
    in
      info.width




  let get matrix i j =
    let info,matrix = matrix
    in
      matrix.(i + info.start_i).(j + info.start_j)




  let set matrix i j value =
    let info,matrix = matrix
    in
      matrix.(i + info.start_i).(j + info.start_j) <- value




  (* creation -------------------------------------------------------------------
   *)
  let create height width =
    { start_i=0; start_j=0; height=height; width=width },
    Array.make_matrix height width Scalar.neutral_add
      



  let create_zero height width = create height width
        



  let create_identity dim =
    let
      result = create_zero dim dim
    in
      for i = 0 to dim - 1 do
        set result i i Scalar.neutral_multiply
      done;
      result
        



  let assign_zero dest =
    for x = 0 to (width dest) - 1 do
      for y = 0 to (height dest) - 1 do 
        (set dest y x Scalar.neutral_add)
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
    let info,matrix = matrix
    in
      { start_i = info.start_i+i;
        start_j = info.start_j+j; 
        height = height;
        width = width 
      },matrix




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
     { start_i = 0;
        start_j = 0; 
        height = Array.length array;
        width = Array.length array.(0)
      },array
    




  let row_vector_of_array array =
    of_array [| array |]




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
            sum = ref Scalar.neutral_add
          in
            for z = 0 to (width mat1) - 1 do
              sum := Scalar.add !sum (Scalar.multiply (get mat1 y z) (get mat2 z x))
            done;
            set result y x !sum
        done
      done;
      result




  let multiply_scalar_inplace mat1 scalar =
    for x = 0 to (width mat1) - 1 do
      for y = 0 to (height mat1) - 1 do
        (set mat1 y x (Scalar.multiply (get mat1 y x) scalar))
      done
    done;
    mat1




  let multiply_scalar mat1 scalar =
    let
      result = create (height mat1) (width mat1)
    in
      for x = 0 to (width mat1) - 1 do
        for y = 0 to (height mat1) - 1 do
          (set result y x (Scalar.multiply (get mat1 y x) scalar))
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
          (set result y x (Scalar.add (get mat1 y x) (get mat2 y x)))
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
        (set mat1 y x (Scalar.add (get mat1 y x) (get mat2 y x)))
      done
    done;
    mat1




  let negate matrix =
    multiply_scalar matrix (Scalar.negate Scalar.neutral_multiply)




  let negate_inplace matrix =
    multiply_scalar_inplace matrix (Scalar.negate Scalar.neutral_multiply)
      



  let subtract mat1 mat2 =
    add mat1 (negate mat2)




  let subtract_inplace mat1 mat2 =
    add_inplace mat1 (negate_inplace mat2)




  (* other operations -----------------------------------------------------------
   *)
  let map f matrix =
    let
      result = create (width matrix) (height matrix)
    in
      for x = 0 to (width matrix) - 1 do
        for y = 0 to (height matrix) - 1 do
          (set result y x (f y x (get matrix y x)))
        done
      done;
      result
    



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
      sum = ref Scalar.neutral_add
    in
      for x = 0 to (width matrix) - 1 do
        sum := (Scalar.add !sum (get matrix x x))
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




  let euclidean_norm_square vector =
    let result = ref Scalar.neutral_add 
    in
      for y = 0 to (height vector) - 1 do
        result := Scalar.add (Scalar.power (get vector y 0) 2.) !result
      done;
      !result
        



  let is_zero matrix = 
    try
      for x = 0 to (width matrix) - 1 do
        for y = 0 to (height matrix) - 1 do
          if (get matrix y x) <> Scalar.neutral_add then
            raise (BoolResult false)
        done
      done;
      true
    with
        BoolResult(x) -> x
          

  (* i/o ------------------------------------------------------------------------
   *)
  let print matrix =
    for y = 0 to (height matrix) - 1 do
      for x = 0 to (width matrix) - 1 do
        Printf.printf "%s " (Scalar.to_string (get matrix y x))
      done;
      print_newline ()
    done




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
end

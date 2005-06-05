let rec monomial_derivative_factor power derivative =
  match derivative with
    | 0 -> 1
    | n -> power * (monomial_derivative_factor (power - 1) (derivative - 1))




(* this function creates a single polynomial spline segment.

   the derivative_lists are lists of the form 
   
   [ 
   function value (0th derivative);
   1st derivative;
   2nd derivative;
   ...
   ]
   
   each can be of arbitrary length, independent of the other. 
   both may be empty. 
   
   this function returns a function that has the given derivatives
   at left_t and right_t, respectively *)
let create_single_spline left_t left_derivative_list right_t right_derivative_list =
  let 
    left_derivatives = (List.length left_derivative_list)
  and
    right_derivatives = (List.length right_derivative_list)
  in
  let
    poly_length = left_derivatives + right_derivatives (* deg + 1 *)
  in
  
  let
    matrix = Matrix.create poly_length poly_length
  and
    vector = Matrix.create poly_length 1
  in

  let fill_matrix_row row x derivative =
    for i = 0 to poly_length - 1 do
      Matrix.set matrix row i 
        (if i - derivative >= 0 then
           (x ** (float_of_int (i - derivative)) *. 
              (float_of_int (monomial_derivative_factor i derivative)))
         else
           0.)
    done
  in

  let rec fill_matrix start_row x derivative value_list =
    match value_list with
      | [] -> ()
      | head::tail ->
          fill_matrix_row start_row x derivative;
          Matrix.set vector start_row 0 head;
          fill_matrix (start_row + 1) x (derivative + 1) tail
  in
  
    (* create a linear system of equations to represent requirements
    on left- and right-hand derivatives (the 0th derivative being
    the function value itself) *)
    fill_matrix 0 left_t 0 left_derivative_list;
    fill_matrix left_derivatives right_t 0 right_derivative_list;
    
    (* solve it for the coefficients *)
    let 
      coefficients = Matrix.list_of_matrix (Matrix.linear_solve matrix vector)
    in
      (* create a function that evaluates the polynomial using Horner's 
      scheme, as in ax^3+bx^2+cx+d = ((ax+b)x+c)x+d *)
      (fun x ->
         let rec horner_step coefficient_list =
           match coefficient_list with
             | coeff :: tail ->
                 coeff +. x *. (horner_step tail)
             | [] -> 0.
         in
           horner_step coefficients)




(* given a list of tuples consisting of an x-coordinate and derivative_lists 
   (see above), it creates a continuous function through all these points 
   that has the given derivatives at the corresponding x coordinates *)
let create_spline parameters =
  let rec create_spline_internal left_t left_derivatives parameters =
    match parameters with 
      | (right_t,right_derivatives) :: tail ->
          (left_t,right_t,
          (create_single_spline left_t left_derivatives right_t right_derivatives)) ::
          create_spline_internal right_t right_derivatives tail
      | [] -> []
  in
    match parameters with 
      | (point,derivatives) :: tail ->
          let 
            intervals_functions = create_spline_internal point derivatives tail
          in
            (* create a piecewise-defined function *)
            (fun x ->
               match List.find 
                 (fun (left,right,f) -> (left <= x && x <= right))
                 intervals_functions
               with
                 | left,right,f -> f x)
                 
      | [] ->
          raise (Failure "create_spline: no point in parameters")




(* given a left and right derivative_list as well as a list of pairs 
   consisting of x and y coordinates, this function "guesses"
   appropriate first derivatives and creates a continuously differentiable
   function through all these points. *)
let create_auto_spline left_t left_derivatives xy_pairs right_t right_derivatives =
  let rec runner left_t left_value this_t this_value list =
    match list with
      | (right_t,right_value) :: tail ->
          (this_t , 
           [ this_value ;
             0.5 *. ((this_value -. left_value) /. (this_t -. left_t) +.
                       (right_value -. this_value) /. (right_t -. this_t)) ])
          :: runner this_t this_value right_t right_value tail
      | [] ->
          [(this_t , 
            [ this_value ;
              0.5 *. ((this_value -. left_value) /. (this_t -. left_t) +.
                        ((List.hd right_derivatives) -. this_value) /. (right_t -. this_t)) ])
          ; (right_t, right_derivatives)]
  in
    
  let parameters = 
    (left_t, left_derivatives) :: 
    match xy_pairs with
      | (first_t,first_value) :: tail ->
          runner left_t (List.hd left_derivatives) first_t first_value tail
      |  [] ->
           [ right_t, right_derivatives ]
  in
    create_spline parameters




(* this function does the same as create_auto_spline for each coordinate *)
let create_auto_matrix_spline left_t left_derivatives xy_pairs right_t right_derivatives =
  let 
    dimension = Matrix.height (List.hd left_derivatives)
  in
  
  let
    functions = Array.make dimension (fun t -> 0.)
  in
    for i = 0 to dimension - 1 do
      let dex = (* dimensional extractor :-) *)
        (fun vec -> (Matrix.get vec i 0))
      in
        functions.(i) <- create_auto_spline
          left_t 
          (List.map dex left_derivatives)
          (List.map 
             (fun (t,vec) -> t,(dex vec)) xy_pairs)
          right_t
          (List.map dex right_derivatives)
    done;
    (fun t ->
       Matrix.column_vector_of_array 
       (Array.map (fun f -> f t) functions))

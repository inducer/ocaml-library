open Utility 




type term = Sum of term list 
            | Product of term list
	    | Quotient of term * term
	    | Sine of term
	    | Cosine of term
	    | ArcSine of term
	    | ArcCosine of term
	    | ArcTangent2 of term * term
	    | Power of term * float
	    | Constant of float
	    | Variable of int




let zero = Constant 0.
let one = Constant 1.
let neg_one = Constant (-1.)
let difference x y = Sum( [ x ; Product ( [ neg_one; y]) ] )

let rec is_constant term =
  match term with
    | Sum(list) -> List.for_all is_constant list
    | Product(list) -> List.for_all is_constant list
    | Quotient(dividend,divisor) -> is_constant dividend && is_constant divisor
    | Sine(argument) -> is_constant argument
    | Cosine(argument) -> is_constant argument
    | ArcSine(argument) -> is_constant argument
    | ArcCosine(argument) -> is_constant argument
    | ArcTangent2(y,x) -> is_constant y && is_constant x
    | Power(argument,exponent) -> is_constant argument
    | Constant(x) -> true
    | Variable(i) -> false
    



let rec to_string term =
  match term with
    | Sum(list) -> 
        "(" ^
        (List.fold_left
	   (fun s t -> 
	      if s = "" then
	        s ^ (to_string t)
	      else
	        s ^ "+" ^ (to_string t)
	   )
	   ""
	   list
	) ^ ")"
    | Product(list) -> 
        "(" ^
        (List.fold_left
	   (fun s t -> 
	      if s = "" then
	        s ^ (to_string t)
	      else
	        s ^ "*" ^ (to_string t)
	   )
	   ""
	   list
	) ^ ")"
    | Quotient(dividend,divisor) -> 
        Printf.sprintf "%s/%s" (to_string dividend) (to_string divisor)
    | Sine(argument) -> Printf.sprintf "sin(%s)" (to_string argument)
    | Cosine(argument) -> Printf.sprintf "cos(%s)" (to_string argument)
    | ArcSine(argument) -> Printf.sprintf "arcsin(%s)" (to_string argument)
    | ArcCosine(argument) -> Printf.sprintf "arccos(%s)" (to_string argument)
    | ArcTangent2(y,x) -> Printf.sprintf "arctan2(%s,%s)" (to_string y) (to_string x)
    | Power(argument,exponent) -> Printf.sprintf "(%s)^%f" (to_string argument) exponent
    | Constant(x) -> string_of_float x
    | Variable(i) -> Printf.sprintf "var%i" i




let rec evaluate variables term =
  match term with
    | Sum(list) -> 
        (List.fold_left 
           (fun x term ->
	      x +. (evaluate variables term))
	   0.
	   list)
    | Product(list) -> 
        (List.fold_left 
           (fun x term ->
	      x *. (evaluate variables term))
	   1.
	   list)
    | Quotient(dividend,divisor) -> (evaluate variables dividend) /. (evaluate variables divisor)
    | Sine(argument) -> sin (evaluate variables argument)
    | Cosine(argument) -> cos (evaluate variables argument)
    | ArcSine(argument) -> asin (evaluate variables argument)
    | ArcCosine(argument) -> acos (evaluate variables argument)
    | ArcTangent2(y,x) -> atan2 (evaluate variables y) (evaluate variables x)
    | Power(argument,exponent) -> (evaluate variables argument) ** exponent
    | Constant(x) -> x
    | Variable(i) -> variables.(i)




let rec derivative variable term =
  match term with
    | Sum(list) -> 
        Sum( List.map (derivative variable) list )
    
    | Product(list) -> 
        Sum(List.map 
	      (fun term -> 
	         Product(List.map
		           (fun inner_term ->
			      if term == inner_term then
			        derivative variable inner_term
			      else
			        inner_term
			   )
			   list
			)
	      ) 
	      list)
    | Quotient(dividend,divisor) -> 
        Quotient(
	  Sum( [ Product( [ derivative variable dividend ; divisor ] );
	         Product( [ Constant(-1.); dividend ; derivative variable divisor] )]),
	  Power(divisor,2.)
	)
    | Sine(argument) -> 
        Product( [ derivative variable argument; Cosine(argument) ] )
    | Cosine(argument) -> 
        Product( [ neg_one; derivative variable argument; Sine(argument) ] )
    | ArcSine(argument) -> 
        Product( [ derivative variable argument; 
	           Quotient( Constant(1.),Power(difference one (Power(argument,2.)),0.5)) ] )
    | ArcCosine(argument) -> 
        Product( [ neg_one; derivative variable argument; 
	           Quotient( Constant(1.),Power(difference one (Power(argument,2.)),0.5)) ] )
    | ArcTangent2(y,x) -> 
        raise (Failure "NYI: derivative of atan2")
    | Power(argument,exponent) -> begin
        match exponent with
	  | 0. -> Constant(0.)
	  | _ -> Product( [ Constant(exponent); derivative variable argument; Power(argument,exponent -. 1.) ] )
      end
    | Constant(x) -> Constant(0.)
    | Variable(i) -> if i = variable then Constant(1.) else Constant(0.)




let simplify zero_tolerance term =
  let rec simplify term =
    if is_constant term then
      let value = evaluate [| |] term
      in
        if abs_float value < zero_tolerance then
          Constant(0.)
        else
          Constant(value)
    else
      let rec simplify_list constants variable_terms list =
        match list with 
          | head :: tail ->
	      let result = simplify head
	      in begin
	          match result with
	            | Constant(x) -> simplify_list (x :: constants) variable_terms tail
		    | x -> simplify_list constants (x :: variable_terms) tail
	        end
	  | [] -> constants,variable_terms
      in
        match term with
          | Sum(list) -> 
	      let sums,non_sums =
	        List.partition 
	          (fun x -> 
		     match x with
		       | Sum(list) -> true
		       | _ -> false
		  )
		  list
	      in
	        if List.length sums <> 0 then
	          simplify 
		    (Sum(
		       non_sums @ 
		       (flat_map 
		          (fun x ->
			     match x with 
			       | Sum(list) -> list
			       | _ -> raise (Failure "algebra: non-sum in sum list"))
			  sums)))
                else
	          let constants,variable_terms = simplify_list [] [] list
	          in
	          let constant = List.fold_left (fun x y -> x +. y) 0. constants
	          in
	          let list =
	            if constant = 0. then
	              variable_terms
	            else
	              Constant(constant) :: variable_terms
	          in begin
	              match List.length list with
	                | 0 -> Constant(0.)
		        | 1 -> List.hd list
		        | _ -> Sum(list)
	            end
          | Product(list) -> 
	      let products,non_products =
	        List.partition 
	          (fun x -> 
		     match x with
		       | Product(list) -> true
		       | _ -> false
		  )
		  list
	      in
	        if List.length products <> 0 then
	          simplify 
		    (Product(
		       non_products @ 
		       (flat_map 
		          (fun x ->
			     match x with 
			       | Product(list) -> list
			       | _ -> raise (Failure "algebra: non-product in product list"))
			  products)))
                else
	          let constants,variable_terms = simplify_list [] [] list
	          in
	          let constant = List.fold_left (fun x y -> x *. y) 1. constants
	          in begin
	              if constant = 0. then
		        Constant 0.
		      else
		        let list =
		          if constant = 1. then
		            variable_terms
		          else
		            Constant(constant) :: variable_terms
		        in begin
	                    match List.length list with
	                      | 0 -> Constant(1.)
		              | 1 -> List.hd list
		              | _ -> Product(list)
	                  end
	            end
          | Quotient(dividend,divisor) -> begin
	      match simplify dividend with
	        | Constant(0.) -> Constant(0.)
	        | x -> 
	            match simplify divisor with
	              | Constant(1.) -> dividend
		      | y -> Quotient(x,y)
	    end
	  | x -> x
  in
    simplify term

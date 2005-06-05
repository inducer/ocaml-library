(* based on algorithms from chapter 10 of Numerical Recipes *)

let minimize_parabola a fa b fb c fc =
  let r = (b -. a) *. (fb -. fc)
  and q = (b -. c) *. (fb -. fa)
  in
  let denominator = r -. q
  in
    if (abs_float denominator) <= 1e-15 then begin
      if fa < fb then begin
        if fa < fc then
	  a
	else
	  c
      end
      else begin (* fa >= fb *)
        if fb < fc then
	  b
	else
	  c
      end
    end
    else
    b -. 
      ( (b -. a) *. r -. (b -. c) *. q)
      /. ( 2. *. denominator )




let golden_ratio = 1.618034




let rec find_minimum_bracket f a b =
  let rec iterate a fa b fb c fc =
    if fb < fc then (* found a bracket *)
      a,b,c
    else
      let parabolic_guess = minimize_parabola a fa b fb c fc
      in
      let fparabolic_guess = f parabolic_guess
      in
      let limit = b +. 100. *. (c -. b)
      in
      let next = c +. golden_ratio *. (c -. b)
      in
        if (b < parabolic_guess && parabolic_guess < c) then begin
	  if fparabolic_guess < fc then
	    b,parabolic_guess,c
	  else if fparabolic_guess < fb then
	    a,parabolic_guess,b
	  else
	    (* screw the parabolic_guess *)
	    iterate b fb c fc next (f next)
	end
	else if (c < parabolic_guess && parabolic_guess < limit) then begin
	  if fparabolic_guess < fc then
	    iterate c fc parabolic_guess fparabolic_guess next (f next)
	  else
	    iterate b fb c fc next (f next)
	end
	else
	  iterate b fb c fc next (f next)
  in
  let fa = f a
  and fb = f b
  in
    if fa < fb then (* downhill towards b *)
      find_minimum_bracket f b a
    else 
      let c = b +. golden_ratio *. (b -. a)
      in 
        iterate a fa b fb c (f c)




let minimize_one_dimension f a b c tolerance =
  let rec_golden_ratio = 1. /. golden_ratio
  in
  let complement_golden_ratio = 1. -. rec_golden_ratio
  in

    (* if not ((a < b && b < c) || (c < b && b < a)) then raise (Failure "minimize_one_dimension: bracket in wrong order");
    if not ((f b) < (f a) && (f b) < (f c)) then raise (Failure "minimize_one_dimension: bracket invalid"); *)
    let rec iterate x0 x1 fx1 x2 fx2 x3 =
      if (abs_float (x3 -. x0)) < tolerance then begin
        if fx1 < fx2 then
          x1
        else
          x2
      end
      else begin
        if fx2 < fx1 then begin
          let next = rec_golden_ratio *. x1 +. complement_golden_ratio *. x3 (* closer to x1 *)
          in
            iterate x1 x2 fx2 next (f next) x3
        end
        else begin
          let next = rec_golden_ratio *. x2 +. complement_golden_ratio *. x0 (* closer to x2 *)
          in
            iterate x0 next (f next) x1 fx1 x2
        end
      end
    in
      if (abs_float (c -. b)) > (abs_float (b -. a)) then begin
        (* [b,c] bigger than [a,b] *)
        let x2 = b +. (1. /. golden_ratio) *. (c -. b)
        in
          iterate a b (f b) x2 (f x2) c
      end
      else begin
        (* [b,c] bigger than [a,b] *)
        let x1 = b -. complement_golden_ratio *. (b -. a)
        in
          iterate a x1 (f x1) b (f b) c
      end



(* straightforward gradient descent, pretty stupid *)  
let minimize_n_dimensions ~f ~df ~start ~tolerance ~max_iterations =
  let rec iterate pos iteration =
    let gradient = df pos
    in
    let one_f = (fun x -> f (Matrix.add pos (Matrix.multiply_scalar gradient x)))
    in
    let a,b,c = find_minimum_bracket one_f 0. (-1e-2)
    in
    let x = minimize_one_dimension one_f a b c tolerance 
    in
    let dpos = Matrix.multiply_scalar gradient x
    in
    let new_pos = Matrix.add pos dpos
    in
      if (Matrix.euclidean_norm dpos) < tolerance || iteration >= max_iterations then
        new_pos
      else
        iterate new_pos (iteration + 1)
  in
    iterate start 0


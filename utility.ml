exception EOF




let pi = acos (-1.)




let square x = x *. x




let empty list =
  match list with
    | head :: tail -> false
    | [] -> true




let head_or value list =
  match list with 
    | head :: tail -> head
    | [] -> value




let join ~conversion ~list ?separator ()=
  let realsep = match separator with
    | None -> ","
    | Some(str) -> str
  in
    match list with 
      | head :: tail -> 
          List.fold_left 
          (fun str item ->
             str ^ realsep ^ (conversion item))
          (conversion head)
          tail
      | [] -> ""




let rec map_or_not ~f ~list =
  match list with
    | [ ] -> [ ]
    | head :: tail ->
        try
	  let 
	    result = f head
	  in
	    result :: map_or_not ~f ~list:tail
        with
          | _ -> map_or_not f tail




let print_bool b =
  match b with
    | true -> print_string "true"
    | false -> print_string "false";;




let rec flat_map ~f ~list =
  match list with
    | head :: tail -> (f head) @ (flat_map ~f ~list:tail)
    | [] -> []




let checked_asin arg =
  if (arg < -. 1.) or (arg > 1.) then
    raise (Failure "asin of invalid number")
  else
    asin arg




let checked_acos arg =
  if (arg < -. 1.) or (arg > 1.) then
    raise (Failure "acos of invalid number")
  else
    acos arg




let extract_last list =
  let rec runner run_list pre_list =
    match run_list with
      | a :: b :: tail -> runner (b :: tail) (a :: pre_list)
      | a :: [] -> pre_list,a
      | [] -> raise (Failure "extract_last: list empty")
  in
    match runner list [] with
      | pre_list,last_element -> List.rev pre_list,last_element




let option_get option =
  match option with
    | None -> raise (Failure "option_get: no value found")
    | Some(x) -> x




let sgn x =
  if x > 0. then
    1.
  else
    if x < 0. then
      (-. 1.)
    else
      0.




let input in_channel max_size =
  let result = String.make max_size ' '
  in
  let size = input in_channel result 0 max_size
  in
    if size = 0 then raise EOF;
    String.sub result 0 size



    
let input_until_eof in_channel =
  let result = ref ""
  and size = ref 0
  in
  let rec loop () =
    try
      result := !result ^ (input in_channel 32768);
      loop ()
    with
      | EOF -> ()
  in
    loop ();
    !result




class [ 'parameter ] signal = object
  val mutable connections = []
  val mutable handle = 0
  
  method connect func =
    connections <- (handle,func) :: connections;
    let func_handle = handle
    in
      handle <- handle + 1;
      func_handle
    
  method disconnect handle = 
    connections <- List.filter (fun x ->
                                  let current_handle,func = x 
				  in
				    if handle = current_handle then
				      false
				    else
				      true)
			       connections
				      
      
  method call (parameter : 'parameter) = 
    List.iter 
      (fun x ->
         let handle,func = x
	 in
	   func parameter)
      connections
end




let argmin ~badness list =
  let rec runner list current_best current_best_badness =
    match list with
    | head :: tail ->
        let head_badness = badness head
        in
          if head_badness < current_best_badness then
            runner tail head head_badness
          else
            runner tail current_best current_best_badness
    | [] -> current_best 

  in
    match list with
    | [] -> raise (Failure "argmin: can't find minimum of no list")
    | head :: tail -> runner tail head (badness head)





let rec list_first_n ~n list =
  match list with
  | [] -> []
  | head :: tail ->
      if n > 0 then
        head :: (list_first_n ~n:(n-1) tail)
      else
        []




let rec list_past_n ~n list =
  match list with
  | [] -> []
  | head :: tail ->
      if n > 0 then
        list_past_n ~n:(n-1) tail
      else
        tail




let collect func =
  let rec runner agg_list =
    try
      runner ((func ()) :: agg_list)
    with
    | _ -> List.rev agg_list
  in
    runner [ ]


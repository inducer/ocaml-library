open Printf

(* data types -----------------------------------------------------------------
*)
exception NoSolution

type 'state node = {
  id : int;
  state : 'state;
  path_cost : float;
  remaining_cost : float;
  total_cost : float;
  parent : 'state node option
}

type 'state operator_result = {
  next_state : 'state;
  cost : float
}




(* tool functions -------------------------------------------------------------
*)
let rec gobble_up_states_to_root ~node ~state_list =
  match node.parent with
    | Some(parent) ->
        (gobble_up_states_to_root
           ~node:parent
           ~state_list:(node.state :: state_list))
    | None -> state_list




(* A* search ------------------------------------------------------------------
*)
let a_star ~initial_state ~generate_successor_states ~estimate_remaining_cost =

  let rec initial_node = 
      {
        id = 0;
        state = initial_state;
        path_cost = 0.;
        remaining_cost = estimate_remaining_cost initial_state;
        total_cost = estimate_remaining_cost initial_state;
        parent = None
      }
  in

  let make_node ~parent op_result =
    let path_cost = op_result.cost +. parent.path_cost
    and remaining_cost = estimate_remaining_cost op_result.next_state 
    in
      {
        id = Random.int(10000000);
        state = op_result.next_state;
        path_cost = path_cost;
        remaining_cost = remaining_cost;
        total_cost = path_cost +. remaining_cost;
         parent = Some(parent)
      } 
  in

  let goal_test node = node.remaining_cost < 1e-6 
  in
  
  let rec inner_a_star ~node_queue =
    let currently_expanded_node = node_queue#pop ()
    in
      if goal_test currently_expanded_node then
        gobble_up_states_to_root ~node:currently_expanded_node ~state_list:[ ]
      else
        let new_nodes = List.map
                          (make_node ~parent:currently_expanded_node)
                          (generate_successor_states currently_expanded_node.state)
        in
          List.iter (fun node -> node_queue#push node) new_nodes;
          inner_a_star ~node_queue
  in

  let node_queue = 
    new Heap.heap 
      ~less_than: (fun a b -> a.total_cost < b.total_cost
                     || (a.total_cost = b.total_cost 
                                           && a.path_cost > b.path_cost))
      ~nothing: initial_node
  in
    node_queue#push initial_node;
    try
      inner_a_star ~node_queue
    with
      | Heap.Empty -> raise NoSolution

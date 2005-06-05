exception NoSolution

type 'state operator_result = {
  next_state : 'state;
  cost : float
}

val a_star :
  initial_state:'state
  -> generate_successor_states:('state -> 'state operator_result list) 
    -> estimate_remaining_cost:('state -> float)
      -> 'state list

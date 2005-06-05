val find_minimum_bracket : (float -> float ) -> float -> float -> float * float * float
val minimize_one_dimension : (float -> float) -> float -> float -> float -> float -> float
val minimize_n_dimensions : 
  f:(Matrix.t -> float) -> 
    df:(Matrix.t -> Matrix.t) -> 
      start:Matrix.t -> 
        tolerance:float -> 
	  max_iterations:int ->
	    Matrix.t

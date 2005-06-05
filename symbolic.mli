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

val is_constant : term -> bool
val to_string : term -> string
val evaluate : float array -> term -> float
val derivative : int -> term -> term
val simplify : float -> term -> term

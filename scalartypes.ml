module FloatScalars = struct
  type t = float
  
  let neutral_add = 0.
  let neutral_multiply = 1.
  let add x y = x +. y
  let multiply x y = x *. y
  let negate x = -1. *. x
  let to_string = string_of_float

  let sin x = sin x
  let cos x = cos x
  let asin x = asin x
  let acos x = acos x
  let atan2 y x = atan2 y x
  let power x y = x ** y
end


module SymbolicScalars = struct
  type t = Symbolic.term
  
  let neutral_add = Symbolic.Constant 0.
  let neutral_multiply = Symbolic.Constant 1.
  let add x y = Symbolic.Sum([x;y])
  let multiply x y = Symbolic.Product([x;y])
  let negate x = Symbolic.Product([Symbolic.Constant (-1.);x])
  let to_string = Symbolic.to_string

  let sin x = Symbolic.Sine(x)
  let cos x = Symbolic.Cosine(x)
  let asin x = Symbolic.ArcSine(x)
  let acos x = Symbolic.ArcCosine(x)
  let atan2 y x = Symbolic.ArcTangent2(y,x)
  let power x y = Symbolic.Power(x,y)
end




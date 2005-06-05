exception Empty

class [ 'a ] heap : 
  less_than:( 'a -> 'a -> bool ) -> 
    nothing : 'a -> 
object
  method size : unit -> int
  method push : 'a -> unit
  method pop : unit -> 'a
end

exception Empty
exception Full

class [ 'a ] ring_queue : capacity : int ->  nothing : 'a -> 
object
  method size : int 

  method push : 'a -> unit
  method pop : unit -> 'a
  method overwrite : 'a -> unit

  method contents : unit -> 'a array
end

  
  


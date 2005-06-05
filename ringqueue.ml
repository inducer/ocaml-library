exception Empty
exception Full

class [ 'a ] ring_queue ~capacity ~nothing = 
object(self)
  val mutable head = 0  
  val mutable tail = 0
  val mutable empty = false
  val mutable store : 'a array = Array.make capacity nothing
                                       
  method size = 
    if head >= tail then
      head - tail
    else
      capacity - tail + head

  method private increment index =
    (index + 1) mod capacity

  method push item =
    if head = tail && not empty then
      raise Full
    else begin
      store.(head) <- item;
      head <- self#increment head;
      empty <- false
    end

  method pop () =
    if empty then
      raise Empty
    else begin
      let result = store.(tail)
      in
      tail <- self#increment tail;
      empty <- (head = tail);
      result
    end

  method overwrite item =
    store.(head) <- item;
    if head = tail && not empty then begin
      if head = tail then
        tail <- self#increment tail;
      head <- self#increment head
    end
    else begin
      head <- self#increment head;
      empty <- false
    end


  method contents () =
    let size = self#size
    in
    if size = 0 then 
      [| |]
    else begin
      let result = Array.make size nothing
      in
      let index = ref tail
      in
      for i = 0 to size - 1 do
        result.(i) <- store.(!index);
        index := self#increment !index
      done;
      result
    end
end;;

  
  

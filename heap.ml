exception Empty

class [ 'a ] heap ~less_than ~nothing = 
object(self)
  val mutable size = 0  
  val mutable array : 'a array ref = ref [| |]
                                       
  method size () = size 

  method push value =
    if size + 1 > (Array.length !array) then begin
      self#reallocate ((size+1)*2)
    end;
    let rec move_up index =
      let parent_index = (index+1)/2-1
      in
        if index <> 0 && less_than value (!array.(parent_index)) then begin
          !array.(index) <- !array.(parent_index);
          move_up (parent_index)
        end
        else
          !array.(index) <- value
    in
      move_up size;
      size <- size + 1
        
  method pop () =
    if size = 0 then
      raise Empty
    else begin
      size <- size - 1;
      let result = !array.(0)
      in
        if size = 0 then
          result
        else begin
          let moving_value = !array.(size)
          in
          let rec move_down tier_start next_tier_start index =
            let index_of_least = ref (-1)
            and least = ref moving_value
            and left_kid = next_tier_start+2*(index-tier_start)
            in
            let right_kid = left_kid + 1
            in
              if (* left child in tree *) left_kid < size then
                if less_than !array.(left_kid) !least then begin
                  index_of_least := left_kid;
                  least := !array.(left_kid)
                end;
              if (* right child in tree *) right_kid < size then 
                if less_than !array.(right_kid) !least then begin
                  index_of_least := right_kid;
                  least := !array.(right_kid)
                end;
              if !index_of_least = -1 then
                (* heap condition satisfied *)
                !array.(index) <- moving_value
              else begin
                (* heap condition not satisfied, recurse *)
                !array.(index) <- !array.(!index_of_least);
                move_down next_tier_start ((2*(next_tier_start+1))-1) !index_of_least
              end
          in
            move_down 0 1 0;
            result
        end
    end

  method private reallocate new_length =
    array := Array.append 
      !array 
               (Array.make (new_length-Array.length !array) nothing)
end;;

  
  

open Bigarray

exception MaxElementsMismatch

class transitive_relation estimate_max_elements = 
object(self)
  val mutable matrix = 
    let result = Array2.create 
                   Bigarray.int 
                   Bigarray.c_layout 
                   estimate_max_elements estimate_max_elements
    in
      Array2.fill result 0;
      result
  
  method matrix () = matrix
  
  method max_elements () = Array2.dim1 matrix

  method copy_from (source : transitive_relation) =
    matrix <- Array2.create 
      Bigarray.int 
      Bigarray.c_layout 
      (Array2.dim1 (source#matrix ())) (Array2.dim2 (source#matrix ()));
    Array2.blit (source#matrix ()) matrix
      
  method is element1 element2 =
    (Array2.get matrix element1 element2) <> 0

  method add element1 element2 =
    if element1 >= (self#max_elements ()) then
      self#reallocate (element1+1);
    if element2 >= (self#max_elements ()) then
      self#reallocate (element2+1);
    
    let max_elements = self#max_elements ()
    in
    let rec inner_add element1 element2 = 
      if (Array2.get matrix element1 element2) = 0 then begin
        Array2.set matrix element1 element2 1;
        for i = 0 to (max_elements - 1) do
          if (Array2.get matrix i element1) <> 0 then
            inner_add i element2;
          if (Array2.get matrix element2 i) <> 0 then
            inner_add element1 i
        done
      end
    in
      inner_add element1 element2;


  method is_irreflexive () =
    let 
      max_elements = self#max_elements ()
    in
    let rec walk_diagonal element =
      if element >= max_elements then
        true
      else
        if (Array2.get matrix element element) <> 0 then
          false
        else
          walk_diagonal (element + 1)
    in
      walk_diagonal 0

  method private print () =
    for x = 0 to (self#max_elements ()) - 1 do
      for y = 0 to (self#max_elements ()) - 1 do
        print_int (Array2.get matrix x y);
      done;
      print_newline ()
    done
        
  method private reallocate min_size =
    print_string "realloc!"; print_newline ();
    let 
      new_size = ref (self#max_elements ()) 
    in
      while !new_size < min_size do
        new_size := !new_size * 2
      done;
      let 
        new_matrix = Array2.create 
                       Bigarray.int 
                       Bigarray.c_layout 
                       !new_size !new_size
      in
        Array2.fill new_matrix 0;
        for e1 = 0 to (self#max_elements ()) - 1 do
          for e2 = 0 to (self#max_elements ()) - 1 do
            Array2.set new_matrix e1 e2 (Array2.get matrix e1 e2)
          done
        done;
        matrix <- new_matrix
      
end

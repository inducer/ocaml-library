open Utility
open Printf
open Str

class command_line argv = object(self)
  val invocation_name = argv.(0)
  val arguments = List.tl (Array.to_list argv)
                          
  method invocation_name () = invocation_name
  
  method get_first_group_of_arguments_matching regexp =
    map_or_not
      (fun arg -> 
         if string_match regexp arg 0 then
           matched_group 1 arg
         else
           raise (Failure "no match"))
      arguments
      
  method exists string =
    not (empty 
           (self#get_first_group_of_arguments_matching 
              (regexp (sprintf "^%s\\(\\)$" (quote string)))))
end

class command_line : string array -> object
  method invocation_name : unit -> string
  method get_first_group_of_arguments_matching : Str.regexp -> string list
  method exists : string -> bool
end

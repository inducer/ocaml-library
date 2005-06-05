class [ 'token ] lexer :
  lexing_function : (unit -> 'token) ->
object
  method get : unit -> 'token
  method forget : unit -> unit
  method peek : unit -> 'token
end

class [ 'token ] lexer ~lexing_function =
object
  val mutable next_token : 'token option = None
  
  method get () =
    match next_token with 
      | None -> 
          lexing_function ()
      | Some(token) ->
          next_token <- None;
          token

  method forget () =
    match next_token with 
      | None -> 
          ignore (lexing_function ());
          ()
      | Some(token) ->
          next_token <- None
  
  method peek () =
    match next_token with 
      | None -> 
          let token = lexing_function ()
          in
            next_token <- Some(token);
            token
          
      | Some(token) ->
          token
end

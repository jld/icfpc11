open Defs
open Speech

let rec loop () =
  utter stdout 0 (Left I);
  ignore (hear stdin);
  loop ()

let _ = 
  try 
    if Sys.argv.(1) = "1" then ignore (hear stdin);
    loop ()
  with 
    End_of_file -> ()
  

open Defs
open Vision

let _ =
  let w = make_world () in
  let me = player_start w in
  Printf.eprintf "Hi!  I'm player %d, and I'm just watching.\n%!" me;
  try
    while true do
      world_dump w;
      zaplog w;
      player_do w 73 (Left I)
    done
  with 
    End_of_file -> ()

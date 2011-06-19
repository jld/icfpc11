open Defs
open Vision
open Goals

let _ =
  Random.self_init ();
  let w = make_world () in
  let me = player_start w in
  let s = make_sched w me in
  Printf.eprintf "Hi!  I'm player %d, and I have no goals in life.\n%!" me;
  try
    while true do
      zaplog w;
      let (i,st) = cycle s in
      player_do w i st
    done
  with
    End_of_file -> ()

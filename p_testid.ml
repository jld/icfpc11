open Defs
open Vision
open Writing

let _ =
  Random.self_init ();
  let w = make_world () in
  let me = player_start w in
  Printf.eprintf "Hi!  I'm player %d, and I'm testing inc and dec.\n%!" me;
  try
    while true do
      let here = Random.int 256
      and there = Random.int 256
      and id = if Random.bool () then Inc else Dec
      in
      let ri = incant (!!id[num there]) in
      List.iter (player_do w here) ri;
      Printf.eprintf "DONE.\n%!";
      world_dump w;
      zaplog w;
    done
  with
    End_of_file -> ()

open Defs
open Vision
open Writing

let _ =
  Random.self_init ();
  let w = make_world () in
  let me = player_start w
  in
  let randlive () =
    let rec look n =
      if n <= 0 then 23 else
      let s = Random.int 256 in
      if w.v.(me).(s) > 0 then s
      else look (pred n)
    in look 256
  in
  let randmove () =
    player_do w (randlive ()) (Left I)
  in
  let recover corpse =
    Printf.eprintf "Leshrac, my liege, grant me the power I am due!\n%!";
    let throne = randlive () in
    List.iter (player_do w throne) (incant (!!Revive[num corpse]))
  in
  try
    while true do
      randmove ();
      world_dump w;
      let l = takelog w in
      List.iter (function
	  Vitals (pl,sl) when pl = me ->
	    if w.v.(me).(sl) <= 0 then
	      recover sl
	| _ -> ()) l;
    done
  with
    End_of_file -> ()

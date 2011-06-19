open Defs
open Vision
open Speech
open Writing
open Alchemy
open Goals
open Tactics

let stuff s =
  slot_alloc_fixed s 0; (* source/target *)
  slot_alloc_fixed s 1; (* help amount *)
  slot_alloc_fixed s 2; (* attack amount *)
  slot_alloc_fixed s 64 (* program *)

let burn1 = iax[pc Help; igetn 0; igetn 0; ax[pc Get; pnum 1];
		pc Attack; igetn 0; igetn 0; ax[pc Get; pnum 2]]
let burn = congeal (describe burn1)
let br = incant burn

let rean = reanim_simple
let juic = juicer_null

let _ =
  let w = make_world () in
  let me = player_start w in
  let s = make_sched w me in
  stuff s;
  let g1 = poly_artifact rean s 1 (fixed_numeric_rite 8192)
  and g2 = poly_artifact rean s 2 (fixed_numeric_rite 512)
  and gpr = poly_artifact rean s 64 (fixed_rite br) in
  add_dep gpr g1;
  add_dep gpr g2;
  let gburn =
    let i = ref 0 and gi = ref None in
    let rec on_ready () =
      match !gi with
	None ->
	  let g0 = poly_artifact rean s 0 ~owned:false
	      (fixed_numeric_rite !i) in
	  gi := Some (g0);
	  NeedHelp [g0]
      | Some (g0) ->
	  if sched_v' s (255 - !i) <= 0 || sched_v s !i < 9096 then begin
	    incr i;
	    if !i > 255 then
	      i := 0;
	    del_goal s g0;
	    gi := None;
	    on_ready ()
	  end else
	    Working
    and on_run () =
      64, irun
    in
    new_goal
      ~name: "P_goalburn.main"
      ~deps: [gpr]
      ~on_ready ~on_run
      s
  in
  ignore gburn;
  try
    while true do
      zaplog w;
      let (i, st) = cycle s in
      player_do w i st
    done
  with
    End_of_file -> ()

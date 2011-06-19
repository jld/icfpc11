open Defs
open Vision
open Goals

let liveness reanimator s p i =
  let g = {
    name = Printf.sprintf "liveness(%d)" i;
    state = Unready;
    deps = [];
    priority = p;
    on_ready = (fun () ->
      if s.world.v.(s.me).(i) > 0 then
	Done
      else
	NeedHelp (reanimator s p i));
    on_run = (fun () ->
      failwith "Tactics.liveness.on_run: shouldn't happen")
  } in
  add_goal s g;
  g

let reanim_null s p i = []

let is_ident s i =
  s.world.f.(s.me).(i) = C I

let performance on_done reanimator s p i ritf =
  let doing = ref [] and todo = ref [] in
  let restart () = 
    doing := ritf ();
    todo := !doing
  in
  restart ();
  let g = {
    name = Printf.sprintf "performance(%d)" i;
    state = Unready;
    deps = [liveness reanimator s p i];
    priority = p;
    on_ready = (fun () ->
      if !todo = [] then
	on_done s i restart
      else
	Working);
    on_run = (fun () ->
      if !todo != !doing && is_ident s i then
	restart ();
      if !todo == !doing && not (is_ident s i) then
	(i, Left Put)
      else
	match !todo with 
	  h::t ->
	    todo := t;
	    (i, h)
	| _ ->
	    failwith "depleted ritual")
  } in
  add_goal s g;
  g

let mono_artifact = 
  performance (fun s i restart -> 
    slot_free s i; Dead)

let poly_artifact =
  performance (fun s i restart ->
    if is_ident s i then (restart (); Working) else Done)

let fixed_rite ri = fun () -> ri

let build_somewhere rean s p ri =
  mono_artifact rean s p (slot_alloc s) (fixed_rite ri)

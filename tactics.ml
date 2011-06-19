open Defs
open Vision
open Goals

let liveness reanimator s p i =
  new_goal
    ~name: (Printf.sprintf "Tactics.liveness(%d)" i)
    ~priority: p
    ~on_ready: (fun () ->
      if sched_v s i > 0 then
	Done
      else
	NeedHelp (reanimator s p i))
    s

let reanim_null s p i = []

let juiciness juicer thresh s p i =
  new_goal
    ~name: (Printf.sprintf "Tactics.juiciness(%d)" i)
    ~priority: p
    ~on_ready: (fun () ->
      if sched_v s i >= thresh then
	Done
      else
	NeedHelp (juicer s p i thresh))
    s

let juicer_null s p i t = []


let is_ident s i =
  sched_f s i = C I

let performance on_done reanimator s p i ritf =
  let todo = ref [] and startedp = ref false in
  let restart () =
    let (nto,nsp) = ritf s i in
    todo := nto;
    startedp := nsp
  in
  restart ();
  new_goal
    ~name: (Printf.sprintf "Tactics.performance(%d)" i)
    ~deps: [liveness reanimator s p i]
    ~priority: p
    ~on_ready: (fun () ->
      if !todo = [] then
	on_done s i restart
      else
	Working)
    ~on_run: (fun () ->
      if !startedp && is_ident s i then
	restart ();
      if not !startedp && not (is_ident s i) then
	(i, Left Put)
      else
	match !todo with 
	  h::t ->
	    todo := t;
	    startedp := true;
	    (i, h)
	| [] ->
	    (i, Left I))
    ~on_remove: (fun () -> slot_free s i)
    s

let mono_artifact = 
  performance (fun s i restart -> Dead)

let poly_artifact =
  performance (fun s i restart ->
    if is_ident s i then (restart (); Working) else Done)

let fixed_rite ri = fun s i -> (ri, false)

let do_somewhere rean s p ri =
  mono_artifact rean s p (slot_alloc s) (fixed_rite ri)

let numeric_rite nf =
  fun s i ->
    let n = (nf ()) in
    match sched_f s i with
      Num m when m == n ->
	([], true)
    | Num m when succ m == n ->
	([Left Succ], true)
    (* Could do more... *)
    | _ ->
	((Writing.incant (Writing.num n)), false)

let fixed_numeric_rite n = numeric_rite (fun () -> n)


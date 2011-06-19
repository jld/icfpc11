open Defs
open Vision
open Goals

let liveness reanimator s ?(p=0) i =
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

let juiciness juicer thresh s ?(p=0) i =
  new_goal
    ~name: (Printf.sprintf "Tactics.juiciness(%d,%d)" i thresh)
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

type rite =
    FullRite of ritual
  | PartialRite of ritual
  | NoRite

let performance on_done reanimator s ?(p=0) i ?(owned=true) ritf =
  let todo = ref [] and startedp = ref false in
  let restart () =
    match ritf s i with
      FullRite ri ->
	todo := ri;
	startedp := false;
	Working
    | PartialRite ri ->
	todo := ri;
	startedp := true;
	Working
    | NoRite ->
	todo := [];
	startedp := false;
	NeedHelp []
  in
  ignore (restart ());
  new_goal
    ~name: (Printf.sprintf "Tactics.performance(%d)" i)
    ~deps: [liveness reanimator s ~p i]
    ~priority: p
    ~on_ready: (fun () ->
      if !todo = [] then
	on_done s i restart
      else
	Working)
    ~on_run: (fun () ->
      if !startedp && is_ident s i then
	ignore (restart ());
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
    ~on_remove: (fun () -> if owned then slot_free s i)
    s

let mono_artifact = 
  performance (fun s i restart -> Dead)

let poly_artifact =
  performance (fun s i restart ->
    if is_ident s i then restart () else Done)

let fixed_rite ri = fun s i -> FullRite ri

let do_somewhere rean s ~p ri =
  mono_artifact rean s ~p (slot_alloc s) (fixed_rite ri)

let numeric_rite nf =
  fun s i ->
    let n = (nf ()) in
    match sched_f s i with
      Num m when m == n ->
	PartialRite []
    | Num m when succ m == n ->
	PartialRite [Left Succ]
    (* Could do more... *)
    | _ ->
	FullRite (Writing.incant (Writing.num n))

let fixed_numeric_rite n = numeric_rite (fun () -> n)


let rec reanim_simple s p i =
  let sp = Writing.A (Writing.C Revive, Writing.num i) in
  [do_somewhere reanim_simple s ~p (Writing.incant sp)]

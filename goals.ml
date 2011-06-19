open Defs
open Vision

type task_state = 
    Unready
  | Blocked
  | Runnable
  | Finished
  | Removing

type goal = {
    name: string;
    mutable refcnt: int;
    mutable state: task_state;
    mutable deps: goal list;
    mutable priority: int;
    on_ready: (unit -> readiness);
    on_run: (unit -> (int * step));
    on_remove: (unit -> unit);
  }
and readiness =
    Done
  | Working
  | NeedHelp of goal list
  | Dead
type sched = {
    world: world; me: int;
    avail: bool array;
    mutable goals: goal list;
    sharing: (string, goal) Hashtbl.t;
  }

let new_goal ~name ?(deps = []) ?(priority = 0)
    ?(on_ready = fun () -> NeedHelp [])
    ?(on_run = fun () -> failwith (name^": nothing to do"))
    ?(on_remove = fun () -> ())
    s =
  try 
    Hashtbl.find s.sharing name
  with
    Not_found ->
      let g = {
	name = name; refcnt = 1; state = Unready;
	deps = deps; priority = priority;
	on_ready = on_ready; on_run = on_run; on_remove = on_remove
      } in
      s.goals <- g::s.goals;
      Hashtbl.add s.sharing name g;
      g

let rec grelease s g =
  g.refcnt <- g.refcnt - 1;
  if g.refcnt <= 0 then del_goal s g
and del_goal s g =
  Hashtbl.remove s.sharing g.name;
  g.refcnt <- 0;
  g.state <- Removing;
  List.iter (grelease s) g.deps;
  g.on_remove ()

let gretain g = g.refcnt <- g.refcnt + 1
let gretained g = gretain g; g

let add_dep requiring required =
  requiring.deps <- (gretained required)::requiring.deps

let findlive s start =
  let rec loop i =
    if i > 255 then 
      failwith "I see dead people?";
    let j = (start + i) mod 256 in
    if s.world.v.(s.me).(j) > 0 then j
    else loop (succ i)
  in loop 0

let idle_loop s =
  new_goal ~name: "Goals.idle_loop"
    ~priority: (-1)
    ~on_ready: (fun () -> Working)
    ~on_run: (fun () -> (findlive s (Random.int 256), Left I))
    s

let make_sched w me =
  let s = { 
    world = w; me = me;
    avail = Array.create 256 true;
    goals = [];
    sharing = Hashtbl.create 17;
  } in 
  let _ = idle_loop s in
  s

let end_phase s =
  let cleanup = List.filter (fun g -> g.state != Removing) in
  s.goals <- cleanup s.goals;
  List.iter (fun g -> g.deps <- cleanup g.deps) s.goals

let untap_phase s =
  List.iter (fun g -> g.state <- Unready) s.goals

let upkeep_phase s =
  let workp = ref true in
  while !workp do
    workp := false;
    List.iter (fun g ->
      if g.state == Unready then
	if List.for_all (fun g -> g.state == Finished) g.deps then begin
	  workp := true;
	  try
	    match g.on_ready () with
	      Done -> g.state <- Finished
	    | Working -> g.state <- Runnable
	    | NeedHelp gs -> g.state <- Blocked; g.deps <- gs@g.deps
	    | Dead -> del_goal s g
	  with
	    e -> (* This will yield a slightly inconsistent state, but... *)
	      Printf.eprintf "Goals.upkeep_phase: %s: exception %s\n%!"
		g.name (Printexc.to_string e);
	      g.state <- Removing
	end) s.goals;
  done

let rec main_phase s =
  let candidates = List.filter (fun g -> g.state == Runnable) s.goals in
  let highest = ref min_int and best = ref None in
  List.iter (fun g ->
    if !highest < g.priority then begin
      highest := g.priority;
      best := Some g
    end) candidates;
  match !best with
    None -> failwith "I have no goals?"
  | Some g ->
      try
	g.on_run ()
      with
	e -> (* This will also yield inconsistency, but... *)
	  Printf.eprintf "Goals.main_phase: %s: exception %s\n%!"
	    g.name (Printexc.to_string e);
	  g.state <- Removing;
	  main_phase s

let cycle s =
  try
    end_phase s;
    untap_phase s;
    upkeep_phase s;
    main_phase s
  with
    e -> (* Emergency safeguard.  We're probably toast, but... *)
      Printf.eprintf "Goals.cycle: exception %s\n%!" 
	(Printexc.to_string e);
      (0, Left I)

let slot_alloc_fixed s i =
  if not s.avail.(i) then
    failwith "You can't have that!";
  s.avail.(i) <- false

let slot_free s i =
  if s.avail.(i) then
    failwith "You already gave me that!";
  s.avail.(i) <- true

let slot_alloc s =
  let start = 32 + Random.int 96 in
  let rec loop i =
    if i > 255 then 
      raise Not_found;
    let j = (start + i) mod 256 in
    if s.avail.(j) && s.world.v.(s.me).(j) > 0 then j
    else loop (succ i)
  in
  let sl = loop 0 in
  slot_alloc_fixed s sl;
  sl


open Defs
open Vision

type task_state = 
    Blocked
  | Runnable
  | Finished
  | Removing

type sched = {
    world: world; me: int;
    avail: bool array;
    mutable goals: goal list;
  }
and goal = {
    mutable state: task_state;
    mutable deps: goal list;
    mutable priority: int;
    on_ready: (sched -> goal -> readiness);
    on_run: (sched -> goal -> (int * step));
  }
and readiness =
    Done
  | Working
  | NeedHelp of goal list
  | Dead

let findlive s start =
  let rec loop i =
    if i > 255 then 
      failwith "I see dead people?";
    let j = (start + i) mod 256 in
    if s.world.v.(s.me).(j) > 0 then j
    else loop (succ i)
  in loop 0

let idle_loop () =
  { state = Blocked;
    deps = [];
    priority = -1;
    on_ready = (fun s g -> Working);
    on_run = (fun s g -> (findlive s (Random.int 256), Left I));
  }

let make_sched w me =
  { world = w; me = me;
    avail = Array.create 256 true;
    goals = [idle_loop ()];
  }

let add_goal s g =
  g.state <- Blocked;
  s.goals <- g::s.goals

let del_goal s g =
  g.state <- Removing

let reap_goals s =
  s.goals <- List.filter (fun g -> g.state != Removing) s.goals

let untap_phase s =
  reap_goals s;
  List.iter (fun g -> g.state <- Blocked) s.goals;
  let workp = ref true in
  while !workp do
    workp := false;
    List.iter (fun g ->
      if g.state == Blocked then
	if List.for_all (fun g -> g.state == Finished) g.deps then begin
	  workp := true;
	  match g.on_ready s g with
	    Done -> g.state <- Finished
	  | Working -> g.state <- Runnable
	  | NeedHelp gs -> g.deps <- gs@g.deps
	  | Dead -> g.state <- Removing
	end) s.goals;
  done

let main_phase s =
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
      g.on_run s g

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
  in loop 0

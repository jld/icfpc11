open Defs
open Vision
open Speech
open Writing
open Alchemy
open Goals
open Tactics

let stuff s =
  slot_alloc_fixed s 0; (* bomb amount *)
  slot_alloc_fixed s 1; (* bomb target *)
  slot_alloc_fixed s 2; (* bomb program *)
  slot_alloc_fixed s 3; (* restore source *)
  slot_alloc_fixed s 4; (* restore target *)
  slot_alloc_fixed s 5; (* restore program *)
  slot_alloc_fixed s 6; (* restore big *)
  slot_alloc_fixed s 7; (* restore little *)
  slot_alloc_fixed s 8  (* help program *)


(* Bomb parts *)
let bombsize = 8192
let restbig = 8192
let restlit = 768

let bomb0 = 
  let svi = ax[pc Succ; V"i"]
  and dvi = ax[pc Dbl; V"i"]
  and g0 = ax[pc Get; pnum 0] in
  let core =
    L("i", ax[pc Attack; V"i"; dvi; g0;
	      pc Attack; svi; dvi; g0;
	      pc Zombie; dvi; dvi]) in
  scompl [core; pc Get; pc Succ]

let bomb1 =
  let svi = ax[pc Succ; V"i"] 
  and g0 = ax[pc Get; pnum 0] in
  let core =
    L("i", ax[pc Attack; V"i"; svi; g0;
	      pc Attack; svi; svi; g0;
	      pc Zombie; svi; svi]) in 
  scompl [core; pc Get; pc Succ]

let bomb0r = incant (describe bomb0)
let bomb1r = incant (describe bomb1)
let bombrun = Right Zero

let snipe l =
  incant (describe (scompl ([ax[pc S; pc Zombie; pc Dec]]
			    @(List.map pc l)@[pc Get; pc Succ])))@[Right Zero]


(* Help parts *)
let help =
  let src = ax[pc Get; V"3"]
  and trg = ax[pc Get; ax[pc Succ; V"3"]]
  and big = ax[pc Get; ax[pc Dbl; V"3"]]
  and lit = ax[pc Get; ax[pc Succ; ax[pc Dbl; V"3"]]] in
  let f3 =
    L("3",
      (ax[pc Help; src; src; big;
	  pc Help; src; trg; lit])) in
  scompl [f3; pc Succ; pc Dbl; pc Succ]

let helpr = incant (describe help)
let restorer = (incant (congeal (!!Get[num 8])))
let helprun = Right Zero

(* State? *)
let urgent = Array.create 256 false
let targetval = Array.create 256 0 (* backwards *)
let note_log s l =
  let me = s.me in
  let taken = ref 1 in
  List.iter (function
      Zombied (pn, sn) when pn != me ->
	targetval.(255 - sn) <- 0;
    | _ -> ()) l;
  List.iter (function
      Gotten (pn, sn) when pn != me ->
	Printf.eprintf "note_log: split %d (%d)\n%!" sn 
	  (targetval.(255 - sn));
	taken := !taken + (targetval.(255 - sn) + 1) / 2;
	targetval.(255 - sn) <- targetval.(255 - sn) / 2
    | _ -> ()) l;
  List.iter (function
      Played (pn, sn, _, _, _) when pn != me ->
	Printf.eprintf "note_log: splat %d (%d)\n%!" sn !taken;
	targetval.(255 - sn) <-
	  if sched_f' s sn = Vision.C I
	  then 0 else targetval.(255 - sn) + !taken;
	taken := 0;
    | _ -> ()) l


(* Bombing. *)
let bombed s i =
  sched_v' s (255 - i) <= 0

let depleted s i =
  sched_v s i < 9096

type bombsight =
    Even
  | Odd
  | Any

let is_target bt i =
  match bt with
    Even -> i mod 2 == 0
  | Odd -> i mod 2 == 1
  | Any -> true

let getbomb = function
    Even -> bomb0r
  | Odd -> bomb1r
  | Any -> failwith "Not a bomb"

let preferred_bomb i = 
  if i mod 2 == 0 then Even else Odd

let rec source_of bt i =
  match bt with
    Even -> i / 2
  | Odd -> i - 1
  | Any -> source_of (preferred_bomb i) i

let depletion_penalty s = 
  if sched_f s 5 = Vision.C I then 40 else 20

let bomb_target s bt = 
  let best = ref (-1)
  and highest = ref min_int
  and dp = depletion_penalty s in
  for i = 0 to 255 do
    if is_target bt i then
      let src = source_of bt i in
      let value = targetval.(i)
	  - (if depleted s src then dp else 0)
	  - (if depleted s (succ src) then dp else 0)
      in
      if !highest < value && not (bombed s i)
      then begin
	best := i;
	highest := value
      end
  done;
  if !best >= 0 then Some !best else None

(* Sniping. *)
let field_ops = ref false
let not_worth_it = Array.create 256 false

let sniper s ~p src targ combl =
  Printf.eprintf "Trying to snipe %d\n%!" targ;
  try 
    let i = slot_alloc s in
    let payload = snipe combl in
    let magazine = ref payload in
    let on_ready () =
      if !magazine = [] then
	(* We actually finished. *)
	Dead
      else if sched_f s 1 <> (Vision.Num src) || sched_v s 1 <= 0 then
	(* If [1] was changed or died, bail. *)
	Dead
      else if sched_v s i <= 0 || (is_ident s i && !magazine != payload) then
    	(* If [i] is dead or tampered with, assume enemy action. *)
	(field_ops := true; Dead)
      else if sched_v' s (255 - targ) > 1 then
	(* If the target was healed, assume they're wise to us. *)
	(field_ops := true; Dead)
      else if targetval.(targ) <= List.length !magazine then
	(* If it's no longer a reasonable target. *)
	(Printf.eprintf "BEES %d\n%!" targ; Dead)
      else
	(* Okay.  I think that's everything that go wrong. *)
	Working
    and on_run () =
      match !magazine with 
	h::t -> magazine := t; (i,h)
      | [] -> (* Shouldn't happen... *) (i, Left I)
    and on_remove () =
      slot_free s i
    in new_goal
      ~name:(Printf.sprintf "P_fancybomb.sniper(%d,%d,%d)" src targ i)
      ~priority:p
      ~on_ready ~on_run ~on_remove
      s
  with
    (* For Not_found, but let's make it a catchall just in case. *)
    _ -> new_goal ~name:"BEES" ~on_ready:(fun _ -> Dead) s

let covert_ops s ~p =
  let worthyp x targ =
    targ >= 0 && targ <= 255 && sched_v' s (255 - targ) == 1 
      && targetval.(targ) > 10 + 3 * x && not (not_worth_it.(targ))
  in
  let on_ready () =
    if !field_ops then Dead else
    match sched_f s 1 with
      Num src ->
	if worthyp 0 src then
	  NeedHelp [sniper s ~p src src []]
	else if worthyp 1 (src + 1) then
	  NeedHelp [sniper s ~p src (src + 1) [Succ]]
	else if worthyp 1 (src * 2) then
	  NeedHelp [sniper s ~p src (2 * src) [Dbl]]
	else 
	  NeedHelp []
    | _ ->
	NeedHelp []
  in new_goal
    ~name: "P_fancybomb.covert_ops"
    ~priority: p
    ~on_ready
    s


(* Contingencies. *)
let rean = reanim_simple
let juicer_flag s p i t = urgent.(i) <- true; []
let repleteness s ?(p=0) i =
  juiciness juicer_flag 9096 s ~p i

let _ =
  Random.self_init ();
  let w = make_world () in
  let me = player_start w in
  let s = make_sched w me in
  stuff s;
  let vsight = ref Any in
  let bomb_rite s i =
    match bomb_target s Any with
      Some targ ->
	let sight = preferred_bomb targ in
	vsight := sight;
	FullRite (getbomb sight)
    | None ->
	NoRite
  in
  let g_bamt = poly_artifact rean s ~p:10 0 (fixed_numeric_rite bombsize)
  and g_rbig = poly_artifact rean s ~p:0 6 (fixed_numeric_rite restbig)
  and g_rlit = poly_artifact rean s ~p:0 7 (fixed_numeric_rite restlit)
  and g_rhlp = poly_artifact rean s ~p:0 8 (fixed_rite helpr)
  and g_rest = poly_artifact rean s ~p:0 5 (fixed_rite restorer)
  and g_bomb = poly_artifact rean s ~p:10 2 bomb_rite
  in
  add_dep g_bomb g_bamt;
  add_dep g_rest g_rhlp;
  let helpslice = ref 0 and vdhelp = ref None in
  let give_slice () =
    helpslice := !helpslice + 3;
    let dhelp = delay s !helpslice in
    vdhelp := Some dhelp;
    take_dep g_bomb dhelp
  and lose_slice () =
    match !vdhelp with
      Some dhelp -> del_goal s dhelp
    | None -> ()
  in
  let g_targ = 
    let vtarg = ref None in
    let rec on_ready () =
      match !vtarg with
	None ->
	  begin match bomb_target s !vsight with
	    None -> 
	      (* Can't find a target for loaded bomb; must dispose of it. *)
	      Working
	  | Some targ ->
	      (* Found a target for loaded bomb; key it in. *)
	      let src0 = source_of !vsight targ in
	      let g_targ = poly_artifact rean s ~p:10 1 ~owned:false
		  (fixed_numeric_rite src0)
	      and g_j0 = repleteness s src0
	      and g_j1 = repleteness s (succ src0) in
	      let deps = [g_targ; g_j0; g_j1] in
	      vtarg := Some (targ, src0, !vsight, deps);
	      NeedHelp deps
	  end
      | Some (targ, src0, osight, gs) ->
	  if bombed s targ || osight <> !vsight 
	  then begin
	    (* Can't bomb this target; try to find another for same bomb. *)
	    List.iter (grelease s) gs; (* XXX *)
	    vtarg := None;
	    on_ready ()
	  end else
	    (* Can bomb; do so. *)
	    Working
    and on_run () =
      match !vtarg with
	Some (targ, src0, osight, gs) ->
	  (* Come from "key it in" via "do so".  Bombs away, pretzel-boy! *)
	  give_slice ();
	  2, bombrun
      | None ->
	  (* Come from "dispose of it". *)
	  2, Left Put
    in new_goal ~name:"P_fancybomb.bombtarget"
      ~deps:[g_bomb] ~priority:10
      ~on_ready ~on_run s
  in
  let g_help =
    let vhelper = ref 0
    and vhelpee = ref 0
    and vghnums = ref None in
    let rec on_ready () =
      match !vghnums with
	None ->
	  let helper = find_slot_where
	      (fun i -> not (depleted s i)) (fun _ -> (-1))
	      (s.world.turn / 100) in
	  vhelper := helper;
	  vhelpee := (-1);
	  let poorest = ref 10000 in
	  for helpee = 0 to 255 do
	    let weight = sched_v s helpee 
		- (if urgent.(helpee) then 65536 else 0) in
	    if weight < !poorest then begin
	      poorest := weight;
	      vhelpee := helpee
	    end
	  done;
	  if !vhelpee < 0 || !vhelper < 0 then begin
	    lose_slice ();
	    NeedHelp [] 
	  end else 
	    let g_helper = poly_artifact rean s 3 ~owned:false
		(fixed_numeric_rite !vhelper)
	    and g_helpee = poly_artifact rean s 4 ~owned:false
		(fixed_numeric_rite !vhelpee)
	    in
	    vghnums := Some (g_helper, g_helpee);
	    take_dep g_helpee (liveness rean s !vhelpee);
	    NeedHelp [g_helper; g_helpee]
      | Some (g_helper, g_helpee) ->
	  if depleted s !vhelper || sched_v s !vhelpee > 10000 then begin
	    if (sched_v s !vhelpee > 10000) then begin
	      urgent.(!vhelpee) <- false
	    end;
	    del_goal s g_helper;
	    del_goal s g_helpee;
	    vghnums := None;
	    on_ready ();
	  end else
	    (* Three times... *)
	    Working
    and on_run () =
      5, helprun
    in
    new_goal ~name:"P_fancybomb.helptarget"
      ~deps:[g_rest; g_rbig; g_rlit]
      ~on_ready ~on_run s
  in
  ignore g_targ;
  ignore g_help;
  ignore (covert_ops s ~p:20);
  try
    while true do
      note_log s (takelog w);
      let (i, st) = cycle s in
      player_do w i st
    done
  with
    End_of_file -> ()

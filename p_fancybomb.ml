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
let nrest = (bombsize + restlit - 1) / restlit

let bomb0 = 
  let svi = ax[pc Succ; V"i"]
  and dvi = ax[pc Dbl; V"i"]
  and g0 = ax[pc Get; pnum 0] in
  let core =
    L("i", ax[pc Attack; V"i"; dvi; g0;
	      pc Attack; svi; dvi; g0;
	      pc Zombie; dvi]) in
  scompl [core; pc Get; pc Succ]

let bomb1 =
  let svi = ax[pc Succ; V"i"] 
  and g0 = ax[pc Get; pnum 0] in
  let core =
    L("i", ax[pc Attack; V"i"; svi; g0;
	      pc Attack; svi; svi; g0;
	      pc Zombie; svi]) in 
  scompl [core; pc Get; pc Succ]

let bomb0r = incant (describe bomb0)
let bomb1r = incant (describe bomb1)
let bombrun = Right Zero


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
  scompl [f3; pc Dbl; pc Dbl; pc Dbl; pc Succ]

let helpr = incant (describe help)
let restorer = (incant (congeal (!!Get[num 3])))
let helprun = Right Zero

(* State? *)
let targetval = Array.create 256 0 (* backwards *)
let note_move i =
  targetval.(255 - i) <- targetval.(255 - i) + 1

(* Bombing. *)
let bombed s i =
  sched_v' s (255 - i) <= 0

let depleted s i =
  sched_v s i < 9096

type bombsight =
    Even
  | Odd
  | Nonzero
  | Any

let is_target bt i =
  match bt with
    Even -> i mod 2 == 0
  | Odd -> i mod 2 == 1
  | Nonzero -> i != 0
  | Any -> true

let getbomb = function
    Even -> bomb0r
  | Odd | Nonzero -> bomb1r
  | Any -> failwith "Not a bomb"

let preferred_bomb i = 
  if i mod 2 == 0 then Even else Odd

let rec source_of bt i =
  match bt with
    Even -> i / 2
  | Odd | Nonzero -> i - 1
  | Any -> source_of (preferred_bomb i) i

let bomb_target s bt = 
  let best = ref (-1)
  and highest = ref (-1) in
  for i = 0 to 255 do
    let src = source_of bt i in
    if targetval.(i) > !highest
	&& is_target bt i
	&& not (bombed s i)
	&& not (depleted s src)
	&& not (depleted s (succ src))
    then begin
      best := i;
      highest := targetval.(i)
    end
  done;
  Printf.eprintf "bomb_target: best=%d highest=%d\n%!" !best !highest;
  if !best >= 0 then Some !best else None

(* Restoring.  *)
(* ... *)

let rean = reanim_simple

let _ =
  Random.self_init ();
  let w = make_world () in
  let me = player_start w in
  let s = make_sched w me in
  stuff s;
  let vsight = ref Nonzero in
  let bomb_rite s i =
    let sight = match !vsight with
      Even | Odd -> Any | sgh -> sgh in
    match bomb_target s sight with
      Some targ ->
	vsight := sight;
	FullRite (getbomb !vsight)
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
		  (fixed_numeric_rite src0) in
	      vtarg := Some (targ, src0, g_targ);
	      NeedHelp [g_targ]
	  end
      | Some (targ, src0, g_targ) ->
	  if bombed s targ || depleted s src0 || depleted s (succ src0) 
	  then begin
	    (* Can't bomb this target; try to find another for same bomb. *)
	    del_goal s g_targ;
	    vtarg := None;
	    on_ready ()
	  end else
	    (* Can bomb; do so. *)
	    Working
    and on_run () =
      match !vtarg with
	Some (targ, src0, g_targ) ->
	  (* Come from "key it in" via "do so".  Bombs away, pretzel-boy! *)
	  targetval.(targ) <- 0;
	  2, bombrun
      | None ->
	  (* Come from "dispose of it". *)
	  2, Left Put
    in new_goal ~name:"P_fancybomb.bombtarget"
      ~deps: [g_bomb]
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
	    if !poorest > sched_v s helpee then begin
	      poorest := sched_v s helpee;
	      vhelpee := helpee
	    end
	  done;
	  if !vhelpee < 0 || !vhelper < 0 then NeedHelp [] else
	  let g_helper = poly_artifact rean s 3 ~owned:false
	      (fixed_numeric_rite !vhelper)
	  and g_helpee = poly_artifact rean s 4 ~owned:false
	      (fixed_numeric_rite !vhelpee)
	  and g_liver = liveness rean s !vhelpee
	  in
	  vghnums := Some (g_helper, g_helpee);
	  add_dep g_helpee g_liver;
	  grelease s g_liver;
	  NeedHelp [g_helper; g_helpee]
      | Some (g_helper, g_helpee) ->
	  if depleted s !vhelper || sched_v s !vhelpee > 10000 then begin
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
(* later: *)
  let note_log = function
      Played (pln, sln, step, cost, ep) when pln != me ->
	note_move sln
    | _ -> ()	
  in try
    while true do
      List.iter note_log (takelog w);
      let (i, st) = cycle s in
      player_do w i st
    done
  with
    End_of_file -> ()

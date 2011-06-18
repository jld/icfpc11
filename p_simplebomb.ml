open Defs
open Speech
open Writing
open Alchemy

(* Slot map:
 * 0 => bomb amount
 * 1 => bomb target
 * 2 => bomb program
 * 3 => restore source
 * 4 => restore target
 * 5 => restore program
 * 6 => restore big
 * 7 => restore little
 *)
let bombsize = 8192
let restbig = 8192
let restlit = 512
let nrest = (bombsize + restlit - 1) / restlit

(* To bomb: f[0] <- bombsize; f[2] <- bombN; f[1] <- target; f[2]/0. *)

let bomb0 = 
  let svi = ax[pc Succ; V"i"]
  and dvi = ax[pc Dbl; V"i"]
  and g0 = ax[pc Get; pnum 0] in
  let core =
    L("i", ax[pc Attack; V"i"; dvi; g0;
	      pc Attack; svi; dvi; g0]) in
  scompl [core; pc Get; pc Succ]

let bomb1 =
  let svi = ax[pc Succ; V"i"] 
  and g0 = ax[pc Get; pnum 0] in
  let core =
    L("i", ax[pc Attack; V"i"; svi; g0;
	      pc Attack; svi; svi; g0]) in 
  scompl [core; pc Get; pc Succ]

let bomb0r = incant (describe bomb0)
let bomb1r = incant (describe bomb1)
let bombrun = Right Zero


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
let restorer = incant (congeal (!!Get[num 3]))
let helprun = Right Zero


(* State! *)
let omoves = Array.create 256 0 (* backwards *)
let depleted = Array.create 256 false
let bombed = Array.create 256 false

let audify (sln, _) =
  omoves.(255 - sln) <- omoves.(255 - sln) + 1

let execute sln st =
  utter stdout sln st;
  audify (hear stdin)

let perform sln = List.iter (execute sln)

(* Restore process in words:
 * 1. perform helpr at 3
 * 2. perform restorer at 5
 * 3. load big/lit constants in 6 and 7, and zero in 3
 * 4. if #3 is depleted, inc 3 and goto 4; else lock it and...
 * 5. find a target; load it into 4
 * 6. trigger 5 enough times
 * 7. release source; release target; inc 3; goto 4
 *)

let shadow3 = ref 65536
let shadow4 = ref 65536

let rest_target () =
  let rec loop i = 
    if i > 255 then 
      None
    else
      if depleted.(i) then
	Some i
      else
	loop (succ i)
  in loop 0

type rest_state =
    Init of (int * ritual) list
  | FindSource
  | LoadTarget of ritual
  | Triggering of int
  | Release
  | Next

let rest_start =
  Init [3, helpr;
	5, restorer;
	6, incant (num restbig);
	7, incant (num restlit)]

let rec rest_sm = function
    Init ((sln,(st::ri))::irl) ->
      Some (sln, st, Init ((sln,ri)::irl))
  | Init ((_,[])::irl) -> 
      rest_sm (Init irl)
  | Init [] ->
      rest_sm Next
  | Next ->
      incr shadow3;
      if !shadow3 > 255 then begin
	shadow3 := 0;
	rest_sm (Init [3, [Left Put]@(incant (num 0))])
      end else
	Some (3, Left Succ, FindSource)
  | FindSource ->
      if depleted.(!shadow3) then
	rest_sm Next
      else begin
	match rest_target () with
	  None ->
	    None
	| Some tg ->
	    shadow4 := tg;
	    (* Printf.eprintf "BEES depl set %d FS\n%!" !shadow3; *)
	    depleted.(!shadow3) <- true;
	    rest_sm (LoadTarget ([Left Put]@(incant (num tg))))
      end
  | LoadTarget (st::ri) ->
      Some (4, st, LoadTarget ri)
  | LoadTarget [] ->
      rest_sm (Triggering nrest)
  | Triggering n when n > 0 ->
      Some (5, helprun, Triggering (pred n))
  | Triggering _ ->
      rest_sm Release
  | Release ->
      (* Printf.eprintf "BEES depl clr %d %d RL\n%!" !shadow3 !shadow4; *)
      depleted.(!shadow3) <- false;
      depleted.(!shadow4) <- false;
      rest_sm Next


(* Bombing:
 * 1. Load f[0] with bombsize.
 * 2. Choose bomb: b0 first time if 0 unused; else parity of highest usable
 * 3. Load f[2] with desired bomb.
 * 4. Choose target; can cheat on parity first time but normally shouldn't?
 * 5. Load f1 with target.
 * 6. Fire.
 * 7. Run helper for half the turns of the target.
 * 8. Goto 2.
 * 9. Comefrom 2 if no target.
 * 10. Run helper for one turn.
 * 11. Goto 2.
 *)

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

let bomb_target bt = 
  let best = ref (-1)
  and highest = ref (-1) in
  for i = 0 to 255 do
    let src = source_of bt i in
    if omoves.(i) > !highest
	&& is_target bt i
	&& not bombed.(i)
	&& not depleted.(src)
	&& not depleted.(succ src)
    then begin
      best := i;
      highest := omoves.(i)
    end
  done;
  if !best >= 0 then Some !best else None


let _ =
  
  if Sys.argv.(1) = "1" then audify (hear stdin);
  let rester = ref rest_start 
  and restslice = ref 0 in
  let step_rester () = 
    match rest_sm !rester with
      None -> false
    | Some (sln, st, nx) ->
	rester := nx;
	execute sln st;
	true
  in
  let rest_once () =
    if not (step_rester()) then
      execute 85 (Left I)
  in
  let sched_rester n =
    (* Printf.eprintf "BEES resting for %d\n%!" n; *)
    let clock = ref n in
    while !clock > 0 do
      if step_rester () then
	decr clock
      else begin
	(* Printf.eprintf "BEES discarding %d\n%!" !clock; *)
	clock := 0
      end
    done
  in
  let bombing_run sight =
    perform 2 (getbomb sight);
    match bomb_target sight with
      None -> (* shouldn't happen? *)
	execute 2 (Left Put);
	0
    | Some targ ->
	let src = source_of sight targ in
	(* Printf.eprintf "BEES depl set %d %d BR\n%!" src (succ src); *)
	depleted.(src) <- true;
	depleted.(succ src) <- true;
	perform 1 (incant (num src));
	execute 2 bombrun;
	bombed.(targ) <- true;
	let advantage = omoves.(targ) in
	omoves.(targ) <- 0;
	execute 1 (Left Put);
	advantage
  in
  perform 0 (incant (num bombsize));
  let rec main firstp =
    match begin 
      if firstp && omoves.(0) == 0 then
	Some Nonzero
      else
	match (bomb_target Any) with
	  None -> None
	| Some i -> Some (preferred_bomb i)
    end with
      None ->
	(* No target; maybe rebomb? *)
	rest_once ();
	main false
    | Some sight ->
	let advantage = bombing_run sight in
	ignore advantage;
	restslice := !restslice + 3 (* XXX magic number *);
	sched_rester !restslice;
	main false
  in
  try
    main true
  with
    End_of_file -> ()

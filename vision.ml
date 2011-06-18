open Defs
open Speech

type value =
    C of card
  | Num of int
  | S1 of value
  | S2 of value * value
  | K1 of value
  | Atk1 of value
  | Atk2 of value * value
  | Hlp1 of value
  | Hlp2 of value * value
  | Zom1 of value

let canon = function
    C Zero -> Num 0
  | v -> v

let denum v =
  match canon v with
    Num n -> n
  | _ -> failwith "not a number"

let rec string_of_value = function
    C c -> Speech.string_of_card c
  | Num i -> string_of_int i
  | S1 v -> "S("^(string_of_value v)^")"
  | S2 (v1,v2) -> "S("^(string_of_value v1)^")("^(string_of_value v2)^")"
  | K1 v -> "K("^(string_of_value v)^")"
  | Atk1 v -> "attack("^(string_of_value v)^")"
  | Atk2 (v1,v2) -> "attack("^(string_of_value v1)^")("^(string_of_value v2)^")"
  | Hlp1 v -> "help("^(string_of_value v)^")"
  | Hlp2 (v1,v2) -> "help("^(string_of_value v1)^")("^(string_of_value v2)^")"
  | Zom1 v -> "zombie("^(string_of_value v)^")"

type playernum = int
type slotnum = int
type cost = int
type exnp = bool
type wlog = 
    Played of playernum * slotnum * step * cost * exnp
  | Vitals of playernum * slotnum
  | Zombied of playernum * slotnum
  | Gotten of playernum * slotnum
  | Stolen of playernum * slotnum

let string_of_log = function
    Played (pl, i, st, co, ep) ->
      let eff = match st with
	Left c -> Printf.sprintf "%s(#%d)" (string_of_card c) i
      | Right c -> Printf.sprintf "#%d(%s)" i (string_of_card c) in
      Printf.sprintf "Player %d did %s, cost %d%s"
	pl eff co (if ep then " and it failed." else ".")
  | Vitals (pl, i) ->
      Printf.sprintf "Player %d had a vitality change at %d." pl i
  | Zombied (pl, i) ->
      Printf.sprintf "Player %d was sent a zombie at %d." pl i
  | Gotten (pl, i) ->
      Printf.sprintf "Player %d retrieved a value from %d." pl i
  | Stolen (pl, i) ->
      Printf.sprintf "Player %d had a value stolen from %d." pl i

type world = {
    mutable prop: int;
    mutable turn: int;
    mutable timer: int;
    mutable zombp: bool;
    v: int array array;
    f: value array array;
    mutable log: wlog list
   }

let make_world () = {
  prop = 0; turn = 0; timer = 1000; zombp = false;
  v = [|Array.create 256 10000;
	Array.create 256 10000|];
  f = [|Array.create 256 (C I);
	Array.create 256 (C I)|];
  log = []
}

let fprop w = w.f.(w.prop)
let vprop w = w.v.(w.prop)
let fopp w = w.f.(1 - w.prop)
let vopp w = w.v.(1 - w.prop)


let sat_succ n = if n < 65535 then succ n else 65535
let sat_dbl n = if n < 32768 then n lsl 1 else 65535

let do_inc n = if n > 0 && n < 65535 then succ n else n
let do_dec n = if n > 0 then pred n else n

let zinc w n = if w.zombp then do_dec n else do_inc n
let zdec w n = if w.zombp then do_inc n else do_dec n

let do_add n a = 
  if n <= 0 then n else if n + a <= 65535 then n + a else 65535
let do_sub n a =
  if n <= 0 then n else if n - a >= 0 then n - a else 0

let zadd w n a = if w.zombp then do_sub n a else do_add n a
let zsub w n a = if w.zombp then do_add n a else do_sub n a


let fget w n =
  if (vprop w).(n) <= 0 then failwith "dead slot";
  let got = (fprop w).(n) in
  w.log <- (Gotten (w.prop, n))::w.log;
  got

let vpay w i n =
  let vp = vprop w in
  if n > vp.(i) then failwith "insufficient vitality";
  vp.(i) <- vp.(i) - n;
  w.log <- (Vitals (w.prop, i))::w.log


let rec apply w f arg =
  if w.timer <= 0 then failwith "timeout";
  w.timer <- w.timer - 1;
  match f with
    C I -> arg
  | C Succ -> Num (sat_succ (denum arg))
  | C Dbl -> Num (sat_dbl (denum arg))
  | C Get -> fget w (denum arg)
  | C Put -> C I
  | C S -> S1 arg
  | S1 x -> S2 (x,arg)
  | S2 (x,y) ->
      let g = apply w x arg in
      let h = apply w y arg in
      apply w g h
  | C K -> K1 arg
  | K1 x -> x
  | C Inc ->
      let vp = vprop w and i = denum arg in
      vp.(i) <- zinc w vp.(i);
      w.log <- (Vitals (w.prop, i))::w.log;
      C I
  | C Dec ->
      let vo = vopp w and i = denum arg in
      vo.(255 - i) <- zdec w vo.(255 - i);
      w.log <- (Vitals (1 - w.prop, 255 - i))::w.log;
      C I
  | C Attack -> Atk1 arg
  | Atk1 i -> Atk2 (i,arg)
  | Atk2 (i,j) ->
      let i = denum i and j = denum j and n = denum arg
      and vo = vopp w in
      vpay w i n;
      vo.(255 - j) <- zsub w vo.(255 - j) (n * 9 / 10);
      w.log <- (Vitals (1 - w.prop, 255 - j))::w.log;
      C I
  | C Help -> Hlp1 arg
  | Hlp1 i -> Hlp2 (i,arg)
  | Hlp2 (i,j) ->
      let i = denum i and j = denum j and n = denum arg
      and vp = vprop w in
      vpay w i n;
      vp.(j) <- zadd w vp.(j) (n * 11 / 10);
      if j != i then
	w.log <- (Vitals (w.prop, j))::w.log;
      C I
  | C Copy ->
      (fopp w).(denum arg)
  | C Revive ->
      let vp = vprop w and i = denum arg in
      if vp.(i) <= 0 then begin
	vp.(i) <- 1;
	w.log <- (Vitals (w.prop, i))::w.log
      end;
      C I
  | C Zombie -> Zom1 arg
  | Zom1 i ->
      let vo = vopp w and i = denum i in
      if vo.(i) > 0 then failwith "not dead yet";
      (fopp w).(255 - i) <- arg;
      vo.(255 - i) <- -1;
      w.log <- (Zombied (1 - w.prop, 255 - i))::w.log;
      C I
  | _ -> failwith "type error"

let play w i st = 
  let exnp = ref false in
  let res =
    try begin match st with
      Left c -> apply w (C c) (fget w i)
    | Right c -> apply w (fget w i) (C c)
    end with
      _ -> exnp := true; C I
  in
  (fprop w).(i) <- res;
  w.log <- (Played (w.prop, i, st, (1000 - w.timer), !exnp))::w.log
  

let pass' w = 
  w.prop <- 1 - w.prop;
  if (w.prop == 0) then
    w.turn <- w.turn + 1;
  w.timer <- 1000

let plan9 w =
  w.zombp <- true;
  let vp = vprop w and fp = fprop w in
  for i = 0 to 255 do
    if vp.(i) < 0 then begin
      begin try
	ignore (apply w fp.(i) (C I));
      with
	_ -> ()
      end;
      fp.(i) <- C I;
      vp.(i) <- 0;
      w.timer <- 1000
    end
  done;
  w.zombp <- false

let pass w =
  pass' w;
  plan9 w

let world_copy w =
  { w with
    v = Array.map Array.copy w.v;
    f = Array.map Array.copy w.f }

let rec list_uniq_a l a =
  match l with
    [] -> List.rev a
  | x::y::t when x = y -> list_uniq_a (x::t) a
  | x::t -> list_uniq_a t (x::a)

let zaplog w =
  w.log <- []

let takelog w =
  let l = w.log in
  zaplog w;
  list_uniq_a (List.sort compare l) []

let world_dump w =
  Printf.eprintf "[[Here's what I think the world looks like:\n";
  for pl = 0 to 1 do
    Printf.eprintf "# Player %d:\n" pl;
    for i = 0 to 255 do
      if w.v.(pl).(i) != 10000 || w.f.(pl).(i) <> C I then
	Printf.eprintf "%d={%d,%s}\n" i
	  w.v.(pl).(i) (string_of_value w.f.(pl).(i));
    done
  done;
  Printf.eprintf "]][[And this is what's happened lately:\n";
  List.iter (fun l -> Printf.eprintf "%s\n" (string_of_log l)) 
    (List.rev w.log);
  Printf.eprintf "]]\n%!"


(* A simple player shell. *)
let player_respond w =
  let (i,st) = hear stdin in
  play w i st;
  pass w

let player_start w =
  let me = int_of_string (Sys.argv.(1)) in
  if me == 1 then
    player_respond w;
  me

let player_do w i st =
  utter stdout i st;
  play w i st;
  pass w;
  player_respond w;

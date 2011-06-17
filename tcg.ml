(* Tree Card Generation *)
open Defs

type tree =
    C of card
  | A of tree * tree

let rec size = function
    C _ -> 1
  | A (t, t') -> (size t) + (size t')

let rec stringp = function
    C _ -> true
  | A (C _, s) 
  | A (s, C _) -> stringp s
  | _ -> false


let rec num n =
  if n == 0 then 
    C Zero
  else if n mod 2 == 0 then
    A (C Dbl, num (n / 2))
  else 
    A (C Succ, num (pred n))

let (!!) p = 
  let rec xp = function
    [] -> C p
  | z::ys -> A ((xp ys), z)
  in
  fun l -> xp (List.rev l)

let comp a b =
  !!S[!!K[a]; b]

let rec cpow a = function
    0 -> C I
  | 1 -> a
  | n -> comp (cpow a (pred n)) a

let crank = function
    A (a, A (b, c)) ->
      A ((comp a b), c)
  | a -> a

let rec rectify = function
    C _ as t -> t
  | A (C c, t) -> A (C c, rectify t)
  | A (t, C c) -> A (rectify t, C c)
  | t -> rectify (crank t)
(* thm: forall t, stringp (rectify t) *)
(* informal thm: forall t1, t2, subexpr (rectify t1) (rectify (A (t1,t2))) *)

type plan =
    Left of card
  | Right of card
  | Splat of card

let rec spool' t a = 
  match t with
    C c -> (Splat c)::a
  | A (C c, t) -> spool' t ((Left c)::a)
  | A (t, C c) -> spool' t ((Right c)::a)
  | _ -> failwith "spool: not a string"
let spool t = spool' t []


type lexp = 
    V of string
  | P of tree
  | L of string * lexp
  | Af of lexp * lexp
  | Ai of string * lexp * lexp

let pc c = P (C c)

let rec is_free v = function
    V v' when v = v' -> true
  | V _ -> false
  | P _ -> false
  | L (v', e) when v = v' -> false
  | L (_, e) -> is_free v e
  | Af (e1, e2) -> is_free v e1 || is_free v e2
  | Ai (v', _, _) when v = v' -> true
  | Ai (_, e1, e2) -> is_free v e1 || is_free v e2

let rec delam = function
    Af (e1, e2) -> Af (delam e1, delam e2)
  | Ai (v, e1, e2) -> Ai (v, delam e1, delam e2)
  | L (v, e) when not (is_free v e) -> Af (pc K, delam e)
  | L (v, V v') when v = v' -> pc I
  | L (v, Af (e, V v')) when v = v' && not (is_free v e) -> delam e
  | L (v, L (v', e)) -> delam (L (v, delam (L (v', e))))
  | L (v, Af (e, e')) 
  | L (v, Ai (_, e, e')) -> Af (Af (pc S, delam (L (v, e))), delam (L (v, e')))
  | e -> e

let rec lower = function
    P t -> t
  | Af (e1, e2)
  | Ai (_, e1, e2) -> A (lower e1, lower e2)
  | _ -> failwith "Unlowerable"

let unlam e = lower (delam e)

(*
type strdiff = 
    Eleft of strdiff
  | Eright of strdiff
  | Change of tree * tree

let rec strdiff s1 s2 =
  match (s1, s2) with
    A (C c1, t1), A (C c2, t2) when c1 == c2 -> Eright (strdiff t1 t2)
  | A (t1, C c1), A (t2, C c2) when c1 == c2 -> Eleft (strdiff t1 t2)
  | _ -> Change (s1, s2)
*)

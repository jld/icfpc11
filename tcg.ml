(* Tree Card Generation *)

type card =
    I
  | Zero
  | Succ
  | Dbl
  | Get
  | Put
  | S
  | K
  | Inc
  | Dec
  | Attack
  | Help
  | Copy
  | Revive
  | Zombie

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

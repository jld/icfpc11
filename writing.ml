open Defs

type spell =
    C of card
  | A of spell * spell

let rec build' l a = 
  match l with
    [] -> a
  | (Left c)::l -> build' l (A (C c, a))
  | (Right c)::l -> build' l (A (a, C c))

let build : ritual -> spell = function
    (Right c)::l -> build' l (C c)
  | l -> build' l (C I)

let rec argue = function
    C c -> [Right c]
  | A (t1, t2) -> [Left K; Left S]@(argue t1)@(argue t2)

let rec incant = function
    C I -> []
  | C c -> [Right c]
  | A (C c, t) -> (incant t)@[Left c]
  | A (t1, t2) -> (incant t1)@(argue t2)


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

let rectify t = build (incant t)


type sigil = 
    V of string
  | P of spell
  | L of string * sigil
  | Af of sigil * sigil
  | Ai of string * sigil * sigil

let pc c = P (C c)
let pnum i = P (num i)
let xlet v e1 e2 = Af (L (v,e2), e1)

let ax = 
  let rec ax' = function
      [] -> pc I
    | [z] -> z
    | z::ys -> Af ((ax' ys), z)
  in
  fun l -> ax' (List.rev l)

let ienv s = L("_get",s)
let irun = Right Get
let iax l = ienv (ax l)
let iget a = Af (V"_get",P a)
let igetn n = iget (num n)

let scomp f g = L("_x", Af(f, Af(g, V"_x")))
let rec scompl = function
    [] -> pc I
  | [f] -> f
  | f::g::l -> scompl ((scomp f g)::l)

let rec entangled v = function
    V v' when v = v' -> true
  | V _ -> false
  | P _ -> false
  | L (v', e) when v = v' -> false
  | L (_, e) -> entangled v e
  | Af (e1, e2) -> entangled v e1 || entangled v e2
  | Ai (v', _, _) when v = v' -> true
  | Ai (_, e1, e2) -> entangled v e1 || entangled v e2

let rec unravel = function
    Af (e1, e2) -> Af (unravel e1, unravel e2)
  | Ai (v, e1, e2) -> Ai (v, unravel e1, unravel e2)
  | L (v, e) when not (entangled v e) -> Af (pc K, unravel e)
  | L (v, V v') when v = v' -> pc I
  | L (v, Af (e, V v')) when v = v' && not (entangled v e) -> unravel e
  | L (v, L (v', e)) -> unravel (L (v, unravel (L (v', e))))
  | L (v, Af (e, e')) 
  | L (v, Ai (_, e, e')) -> Af (Af (pc S, unravel (L (v, e))), unravel (L (v, e')))
  | e -> e

let rec condense = function
    P t -> t
  | Af (e1, e2)
  | Ai (_, e1, e2) -> A (condense e1, condense e2)
  | _ -> failwith "uncondensable sigil"

let describe e = condense (unravel e)

open Defs
open Writing

let rec calcine = function
    C c ->
      A(C I, C c)
  | A (t1, A(t2, t3)) ->
      calcine (A (A (A (C S, A (C K, t1)), t2), t3))
  | t -> t

let dissolve = function
    A(t, C c) -> (t,c)
  | _ -> failwith "insoluble"

let prepare t = dissolve (calcine t)

let congeal (f,x) =
  let sg = Af(L("m",Af(V"m",V"m")),
	      L("p",L("x",
		      (Af (Af (P f, V "x"),
			   (Ai ("x", V "p", V "p"))))))) in
  (describe sg, x)

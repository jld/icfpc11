open Defs
open Writing

let congelation sg =
  Af(L("m",Af(V"m",V"m")),
     L("p",L("x",
	     (Af (Af (sg, V "x"),
		  (Ai ("x", V "p", V "p")))))))

let congeal f =
  describe (congelation (P f))

let congealer =
  describe (L("f", congelation (V "f")))

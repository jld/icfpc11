open Defs
open Writing

let congeal f =
  let sg = Af(L("m",Af(V"m",V"m")),
	      L("p",L("x",
		      (Af (Af (P f, V "x"),
			   (Ai ("x", V "p", V "p"))))))) in
  describe sg

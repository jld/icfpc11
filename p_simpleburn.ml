open Defs
open Speech
open Writing
open Alchemy

let rec perform i = function
    [] -> ()
  | st::ri ->
      utter stdout i st;
      ignore (hear stdin);
      perform i ri

let burn1 = iax[pc Help;igetn 0; igetn 0; igetn 1;
		pc Attack; igetn 0; igetn 0; igetn 2]
let burn = congeal (describe burn1)
let br = incant burn

let _ =
  try
    if Sys.argv.(1) = "1" then ignore (hear stdin);
    perform 85 br;
    perform 1 (incant (num 8192));
    perform 2 (incant (num 512));
    while true do
      perform 0 (incant (num 0));
      for i = 0 to 255 do
	if i != 0 then
	  perform 0 [Left Succ];
	for i = 1 to 33 do
	  perform 85 [irun]
	done;
      done;
      perform 1 [Left Dbl];
      perform 2 [Left Dbl]
    done
  with
    End_of_file -> ()

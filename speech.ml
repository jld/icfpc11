open Defs

let string_of_card = function
    I -> "I"
  | Zero -> "zero"
  | Succ -> "succ"
  | Dbl -> "dbl"
  | Get -> "get"
  | Put -> "put"
  | S -> "S"
  | K -> "K"
  | Inc -> "inc"
  | Dec -> "dec"
  | Attack -> "attack"
  | Help -> "help"
  | Copy -> "copy"
  | Revive -> "revive"
  | Zombie -> "zombie"

let card_of_string = function
    "I" -> I
  | "zero" -> Zero
  | "succ" -> Succ
  | "dbl" -> Dbl
  | "get" -> Get
  | "put" -> Put
  | "S" -> S
  | "K" -> K
  | "inc" -> Inc
  | "dec" -> Dec
  | "attack" -> Attack
  | "help" -> Help
  | "copy" -> Copy
  | "revive" -> Revive
  | "zombie" -> Zombie
  | s -> failwith ("Bad card name "^s)

let utter fp i st =
  match st with
    Left c -> Printf.fprintf fp "1\n%s\n%d\n%!" (string_of_card c) i
  | Right c -> Printf.fprintf fp "2\n%d\n%s\n%!" i (string_of_card c)

let speak fp i =
  List.iter (utter fp i)

let record s i ri =
  let fp = open_out s in
  speak fp i ri;
  close_out fp

let hear fp =
  match input_line fp with
    "1" -> 
      let card = card_of_string (input_line fp) in
      let slot = int_of_string (input_line fp) in
      (slot, Left card)
  | "2" ->
      let slot = int_of_string (input_line fp) in
      let card = card_of_string (input_line fp) in
      (slot, Right card)
  | l ->
      failwith ("Bad side "^l)

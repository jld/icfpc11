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


type player = Human | Comput

(* Next turn *)
let next = function
  | Human -> Comput
  | Comput -> Human

let player2s = function
  | Human -> "Human"
  | Comput -> "Comput"


(* Helper functions on matrices *)

type 'a matrix = 'a array array

let clone_matrix m =
  let cloned = Array.copy m in
  Array.iteri (fun i line -> cloned.(i) <- Array.copy line) cloned ;
  cloned

exception Found of int * int

let find_cell m p =
  try
    for row = 0 to Array.length m - 1 do
      let line = m.(row) in
      for col = 0 to Array.length line - 1 do
        if p line.(col) then raise (Found (row, col))
      done ;
    done;
    None

  with Found (r,c) -> Some (r,c)

let line2s v2s line = Array.fold_left (fun s v -> s ^ v2s v ^ "|") "|" line

let linesep = "-------\n"

(* Transforms a grid to a string. *)
let matrix2s m v2s =
  if Array.length m = 0 then "(empty matrix)"
  else
    let firstline = line2s v2s m.(0) in
    let linesep = (String.make (String.length firstline) '-') ^ "\n" in
    Array.fold_left (fun s line -> s ^ (line2s v2s line) ^ "\n" ^ linesep) linesep m


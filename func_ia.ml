open Game
open Gamebase
open Functory.Network
open Functory.Network.Same
open Game_ia

(* FUNCTORY *)
(* We choose to only parallelize the first moves, i.e. we compute on different workers the very next move the IA could play, it is ideal when the width of the grid is equal to the number of workers (one move by column = one worker) *)

(* to be modified by hand if needed *) 
let () = Functory.Control.set_debug false


(* the map computes one of the available moves from the state after the move and returns the best one *) 
let zemap currentstate = 
  let simul = best_move currentstate in
  match simul with
  | (Some mov,res) -> (Some (last_move currentstate), res)
  | _ -> simul


(* the fold compares the new result with its accumulator and keeps the best result as the new accumulator,
 * it is associative and commutative *)
let zefold best_acu rm = 
  let (_,result_map) = rm in
  let (_,result_acu) = best_acu in
  match (compare Comput result_acu result_map) with
  | Smaller -> best_acu
  | (Equal | Greater | Greatest) -> rm


(* func is launched by the master only *)
let func state =

  (* Worker declarations 
   * Must be edited to add workers *)
  declare_workers ~n:2 "localhost" ;
  declare_workers ~n:2 "geitp107-12" ;


  let moves =  List.filter (is_valid state) (all_moves state) in
  let states = List.map (play state) moves in
  let neutral_element = (None, worst_for(Comput)) in (* Every other tuple will be "better" according to zefold *)

  let output = map_fold_ac ~f:zemap ~fold:zefold neutral_element states in (* this is where the magic happens *)

  Printf.printf "\nComputation done\n%!"; output





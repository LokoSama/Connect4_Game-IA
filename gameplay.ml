open Gamebase
open Game

(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)
let ask_move state =
  let zemove = Graphic_output.choose_move () in
  if not (is_valid state zemove) then (* True if the player tries to play in a full column *)
    begin
      Printf.printf "\n This move is invalid: %s\n\n%!" (move2s zemove) ;
      None
    end
  else Some zemove

(* Get the move from the IA. *)
let ia_move state with_func =
  let t_initial = Unix.gettimeofday() in
  let (mov, res) =
    match with_func with
    | true -> Func_ia.func state
    | false -> Game_ia.best_move state
  in
  let t_ecoule = Unix.gettimeofday() -. t_initial in
  Printf.printf "Expected result: %s\nExecution time: %f s\n\n%!" (result2s res) t_ecoule ;
  match mov with
  | None -> failwith "Game ends."
  | Some m -> m

(*** Each player in turn. ***)

let rec run with_ia with_func state =

  (* Print state & which player to play. *)
  Printf.printf "\n%s to play.\n\n%!" (player2s (turn state)) ;
  Graphic_output.print_state state;

  match result state with
  | Some r ->
    (* Game is finished. Print result. *)
    Printf.printf "*** %s ***\n%!" (result2s r) ;
    let line = read_line() in (* These are to create a pause before the program exits *)
    Printf.printf "%s\n" line;
    ()

  | None ->
    (* Game is not finished. Play one turn. *)

    let state' =
      if with_ia && turn state = Comput
      then play state (ia_move state with_func)
      else
        begin match ask_move state with
          | None -> state (* Invalid move, play same state again. *)
          | Some mov -> play state mov
        end
    in
    run with_ia with_func state'

(* Usage : ./gameplay.native USE_FUNC MASTER 
   Examples : ./gameplay.native true master
           ./gameplay.native true
           ./gameplay.native false
*)

let () =  
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with

  | [| _ ; "useFunc" ; "master" |] ->
    Printf.printf "I am the master.\n\n%!" ;
    Graphic_output.init ();
    run true true initial

  | [| _ ; "useFunc" ; "worker" |] ->
    Printf.printf "I am a poor slave.\nI will do whatever you please, master.\n\n%!" ;
    Functory.Network.Same.Worker.compute ()

  | [| _ ; "noFunc" |] ->
    Printf.printf "Functory disabled.\n\n%!" ;
    Graphic_output.init ();
    run true false initial

  |  _ ->
    Printf.printf "\nInvalid arguments.\nUsage : ./gameplay.native noFunc | (useFunc (master | worker))\n\n%!" ;
    ()


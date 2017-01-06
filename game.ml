open Gamebase

(* This is needed for the graphic_output ; it is not abstract any longer *)

type token = Empty | Red | Yellow 

(* These types are abstract in game.mli *)

type move = int (* column number*)

type state = { grid : token matrix ; player :  player; lastm : move }

type result = Win of player | Even

(* You have to provide these *)
(* parametres for the grid *)
let grid_width = 4
let grid_height = 4

let grid_dimensions =
  (grid_height,grid_width) (* rows , columns *)

(* puts all possible moves in a list *)
let all_moves state = 
  let rec create_list aux_width =
    if aux_width = grid_width then []
    else aux_width :: create_list (aux_width + 1)
  in
  create_list 0


(* Printers *)
let v2s tok =
  match tok with
  |Red -> "R"
  |Yellow -> "Y"
  |Empty -> " "

let rec list2s l =
  if l = [] then ""
  else Printf.sprintf " %d%s" (List.hd l) (list2s (List.tl l))

(* kept for legacy reasons *)
let state2s state = Printf.sprintf "Current grid = \n%s%s\n" (matrix2s state.grid v2s) (list2s (all_moves state))

let move2s n = Printf.sprintf " column %d" n

let result2s r =
  match r with
  | Win p -> (player2s p) ^ " wins"
  | Even -> "Even game"

(* Reader *)
let readmove s = try Some (int_of_string s) with _ -> None


(* creates an initial and empty grid and returns the initial state
 * Change parameters below to determine who goes first *)
let initial =
  (* fill the grid with empty cells *)
  let grid_construct = function
    | (x,y) when (x > 3 && y > 3) -> Array.make_matrix x y Empty
    | _ -> failwith "wrong parameters for grid construction, must be 4x4 at least"
  in
  {	
    grid = grid_construct grid_dimensions;
    player = Human;
    lastm = -1
  }

(* getters for the graphic_output *)
let grid_of_state state =
  state.grid

let last_move state =
  state.lastm

let turn state =
  state.player

let is_valid state m =
  if m < 0 || m >= grid_width then false
  else
  if state.grid.(0).(m) = Empty then true (* we select the first line of the chosen column *)
  else false  

let colorp player =
  match player with
  |Human -> Red
  |Comput -> Yellow

(* used to play one turn *)
let play state m = 

  if is_valid state m then  
    let rec loop state i = (* drop token in the column *)
      if state.grid.(grid_height-i).(m) = Empty then
        let newgrid = clone_matrix state.grid in
        newgrid.(grid_height-i).(m) <- colorp (turn state); 
        {grid = newgrid; player = next state.player; lastm = m}
      else loop state (i+1)
    in   
    loop state 1 
  else
    failwith "invalid move given to function play"


let grid_full state = (* detect if the grid is full *)
  let rec loop acu = 
    if acu = grid_width then true
    else
    if state.grid.(0).(acu) = Empty then false
    else loop (acu+1)
  in
  loop 0

let result state =
  if state.lastm = -1 then None else (* special case for the first round only *)

    let cell_in_grid (lin,col) = (* checks if a given cell is in range of the grid *)
      if lin < 0 || col < 0 || lin >= grid_height || col >= grid_width then false else true
    in

    let get_last_pos = (* uses the state just above. /!\ crashes if invoked with no tokens on the lastm column /!\ *)
      let rec get_last_pos_aux acu =
        match state.grid.(acu).(state.lastm) with
        | Empty -> get_last_pos_aux (acu+1)
        | _ -> (acu,state.lastm)
      in get_last_pos_aux 0
    in

    (* movements vector (lin,col) *) 
    let diag1=(1,-1) in (* down and left *)
    let diag2=(1, 1) in (* down and right *)
    let horiz=(0, 1) in (* to the right *)
    let verti=(1, 0) in (* down *)

    (* send the start point of the search following the axis given *)
    let start (lin,col) (vectx,vecty) =
      if cell_in_grid (lin-3*vectx,col-3*vecty) then (lin-3*vectx,col-3*vecty) else
      if cell_in_grid (lin-2*vectx,col-2*vecty) then (lin-2*vectx,col-2*vecty) else
      if cell_in_grid (lin-1*vectx,col-1*vecty) then (lin-1*vectx,col-1*vecty) else
        (lin,col)
    in

    let rec detectwin (lin,col) (vectx,vecty) acu cpt =
      if acu = 4 then true
      else

      if lin >= grid_height || col < 0 || col >= grid_width || cpt = 7 then false (* if the current cell is out of range, stop the search *)
      else
      if state.grid.(lin).(col) = colorp (next (turn state)) then
        detectwin (lin+vectx,col+vecty) (vectx,vecty) (acu+1) (cpt+1)
      else
        detectwin (lin+vectx,col+vecty) (vectx,vecty) 0 (cpt+1)
    in 

    let aux_pos = get_last_pos in
    if detectwin (start aux_pos verti) verti 0 0 || detectwin (start aux_pos horiz) horiz 0 0 || detectwin (start aux_pos diag1) diag1 0 0 || detectwin (start aux_pos diag2) diag2 0 0 (*testing the 4 axis *)
    then Some (Win (next (turn state)))
    else
    if grid_full state then Some (Even)
    else None 

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller | Greatest

let compare player r1 r2 =  

  match (r1,r2) with
  | (_,Win someone) -> if someone = player then Greatest else Smaller (* Greatest is better than Greater because it refers to the best result possible for player *)
  | (Win someone, _) -> if someone = player then Smaller else Greater
  | (Even,Even) -> Equal

let worst_for p= 
  match p with
  | Human -> Win Comput
  | _-> Win Human


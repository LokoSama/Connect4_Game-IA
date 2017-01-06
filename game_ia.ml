open Game

let rec best_move state =
  match result state with
  | Some x -> (None, x)
  | None ->

    let l = List.filter (is_valid state) (all_moves state) in

    let rec loop movesList best_move_found best_result_found =
      match movesList with
      | []	-> (best_move_found,best_result_found) (* return our best move found if there are no other moves left *)
      | currentMove :: remainingMoves -> 

        let player = turn state in
        let (_,res) = best_move (play state currentMove) in (* res := the result if the ennemy plays it perfectly *)
        match (compare player best_result_found res) with
        | Smaller -> loop remainingMoves best_move_found best_result_found
        | Equal -> loop remainingMoves best_move_found best_result_found
        | Greater -> loop remainingMoves currentMove res
        | Greatest -> (currentMove,res) (* If we find the best move ever for player =" Greatest", we stop computing and send it back *)

    in

    let (x,y) = loop l (List.hd l) (worst_for (turn state)) in
    (Some x,y)







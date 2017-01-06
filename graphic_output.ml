open Graphics
open Game


(* Graphic parameters *)
let cell_dim = 61 (* cell dimension in pixel, keep the number odd so we can have a defined center *)
let window_offset_x = 500 (* window position on the screen: lower left is (0,0), lower right is for instance (1920,0) *)
let window_offset_y = 300
let window_title = "Connect 4"

(* Other, non-modifiable values *)
let (grid_height, grid_width) = grid_dimensions
let window_width = grid_width * cell_dim
let window_height = grid_height * cell_dim + cell_dim (* one additional line for the pointer *)

(* this string is used to open the window *)
let config_string = Printf.sprintf " %dx%d+%d+%d" window_width window_height window_offset_x window_offset_y

(* Initialization function, to be called only once
 * It opens the graph *)
let init () =
  Printf.printf "Move the pointer using 1 for left and 3 for right.\nUse 2 to drop your token.\n%!";
  Graphics.open_graph config_string;
  Graphics.set_window_title window_title;
  ()

(* This function generates (and subsequently returns) an image of a cell 
 * It requires a Graphics.color to be passed as argument *)
let cell_image gr_color =
  let sq x = x*x in (* mostly for lisibility and because x**2 only works for floats *)
  let cell_center = (cell_dim - 1) / 2 in
  let distance_to_center x y = sqrt ( float_of_int( sq (cell_center - x) + sq (cell_center - y) ) ) in
  (* We generate a color matrix and then make an image out of it *)
  let cell_matrix = Array.make_matrix cell_dim cell_dim Graphics.blue in
  let rec draw_token i j =
    if distance_to_center i j < float_of_int cell_center *. 0.78 then (* 0.78 is arbitrary *)
      cell_matrix.(i).(j) <- gr_color;
    if i < (cell_dim-1) then draw_token (i+1) j
    else if j < (cell_dim) then draw_token 0 (j+1)
    else ()
  in
  draw_token 0 0;
  cell_matrix

(* We generate the 3 kinds of cell matrixes we need once and for all to avoid doing it multiple times *)
let red_cell = cell_image Graphics.red
let yellow_cell = cell_image Graphics.yellow
let empty_cell = cell_image Graphics.white


let mkImg = Graphics.make_image

(* This function draws a single cell of the chosen color. 
 * The arguments line and column work like on the token matrix ((0,0) up left, (0,6) up right) *)
(* The argument color can be Empty, Red or Yellow (based on the token colors). *)
let draw_cell line column color =  

  let cell = (* Pick the right cell according to the color *)
    match color with   
    | Red -> ref(red_cell)
    | Yellow -> ref(yellow_cell)
    | Empty -> ref(empty_cell)
  in
  (* Draw the cell right away.
   * We use a ref here to avoid storing multiple instances of the cell *)
  Graphics.draw_image (mkImg !cell) (column * cell_dim) (window_height - cell_dim * (line+2))

(* Draws the given grid *)
let draw_grid grid =
  let rec draw_grid_rec lin col =
    if lin = grid_height then
      ()
    else if col = grid_width then
      draw_grid_rec (lin+1) 0
    else
      let token_color = grid.(lin).(col) in
      draw_cell lin col token_color;
      draw_grid_rec lin (col+1)
  in
  draw_grid_rec 0 0

let print_state state =
  draw_grid (grid_of_state state)


(** Primitive GUI for chosing the column you wish to play in **)

(* Pointer creation and drawing functions *)

(* We need a pointer image so let's build its color matrix
 * This gets a bit messy and I hardly can include the paint file
 * I used for designing and for not getting lost.
 * This pointer is meant to scale with cell_dim.
 * It faces downwards and is made of a "trunk" and a "head". *)

let make_pointer_matrix () =

  (* predicates to check if a set of coordinates is part of the arrow or not 
   * The coordinates are taken from the top-left down like (x,y) <-> (col,lin) *)
  let f = float_of_int in
  let cell_dimf = f cell_dim in

  let x_in_trunk x = cell_dimf *. 0.4 < f x && f x < cell_dimf *. 0.60 in  (* trunk part *)
  let y_in_trunk y = cell_dimf *. 0.2 < f y && f y < cell_dimf *. 0.52 in
  let xy_in_head x y =
    cell_dimf *. 0.3 <= f x && f x < cell_dimf *. 0.7 &&       (* delimit a rectangular zone *)
    cell_dimf *. 0.52 <= f y && f y < cell_dimf *. 0.92 &&
    abs_float(f x -. cell_dimf /. 2.) <= (cell_dimf *. 0.92 -. f y) /. 2. (* then do some improvised math on it to make an arrowhead *)
  in
  let cell_matrix = Array.make_matrix cell_dim cell_dim Graphics.white in
  let rec make_pointer lin col =
    if lin = cell_dim then
      ()
    else if col = cell_dim then
      make_pointer (lin+1) 0
    else
    if (x_in_trunk lin && y_in_trunk col) || xy_in_head lin col then begin
      cell_matrix.(col).(lin) <- Graphics.blue;
      make_pointer lin (col+1)
    end
    else
      make_pointer lin (col+1)
  in
  make_pointer 0 0;
  cell_matrix

let pointer_matrix = make_pointer_matrix ()

(* Draws the pointer on top of the given column *)  
let draw_pointer column =
  Graphics.draw_image (mkImg pointer_matrix) (column * cell_dim) (window_height - cell_dim)

(* Draws a white space on top of the given column, effectively erasing the pointer *)
let erase_pointer column =
  let blank_cell = Graphics.make_image (Array.make_matrix cell_dim cell_dim Graphics.white) in
  Graphics.draw_image blank_cell (column * cell_dim) (window_height - cell_dim)

type direction = Left | Right

(* Moves the pointer and returns its new column *)
let move_pointer currentcolumn dir =
  if (currentcolumn = 0 && dir = Left) || (currentcolumn = (grid_width-1) && dir = Right) then
    currentcolumn
  else
    let i = match dir with
      | Left -> -1
      | Right -> 1
    in
    erase_pointer currentcolumn;
    draw_pointer (currentcolumn + i);
    currentcolumn + i

let cleanup () =
  let rec cleanup_rec i =
    if i > grid_width then
      ()
    else begin
      erase_pointer i;
      cleanup_rec (i+1)
    end
  in
  cleanup_rec 0

let choose_move () =
  let rec loop current_col =
    let keyb_status = Graphics.wait_next_event [Key_pressed] in
    if keyb_status.key = '2' then
      current_col
    else if keyb_status.key = '1' then
      loop (move_pointer current_col Left)
    else if keyb_status.key = '3' then
      loop (move_pointer current_col Right)
    else
      loop current_col
  in
  cleanup ();
  draw_pointer (grid_width / 2);
  loop (grid_width / 2)












(*EOF*)

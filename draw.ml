open Graphics

let block = 50
let gblock = 3*block
              
let graphics_open () =
  open_graph (Printf.sprintf " %dx%d" (4*gblock) (3*gblock));
  set_color black;
  fill_rect 0 0 (4*gblock) (3*gblock);
  set_window_title "Light's out"

let orange = rgb 255 165 0
                   
let aff_of_color c = match c with
  | Biks.White -> white
  | Biks.Yellow -> yellow
  | Biks.Green -> green
  | Biks.Red -> red
  | Biks.Blue -> blue
  | Biks.Orange -> orange

let convert_square n =
  n/3, n mod 3
                    
let square n c =
  let y,x = convert_square n in
  let yg = if n=0 then 2*gblock
           else if n<5 then gblock
           else 0 in 
  let xg = if n=0 || n=5 || n=2 then gblock
           else if n=1 then 0
           else 2*gblock in
  set_color (aff_of_color c);
  fill_rect (xg+x) (yg+y) block block
                   
let _ =
  let cube = Biks.cube in
  graphics_open();
  Array.iteri (fun i x -> square i x) cube;
  (*set_color blue;
  fill_rect 0 0 gblock gblock;
  set_color yellow;
  fill_rect 0 gblock gblock gblock;
  set_color orange;
  fill_rect 0 (2*gblock) gblock gblock;
   *)
  Unix.sleep 5;
  close_graph ()

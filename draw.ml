open Graphics
open Biks
       
let block = 30
let gblock = 3*block
              
let graphics_open () =
  open_graph (Printf.sprintf " %dx%d" (4*gblock) (3*gblock));
  set_window_title "Light's out"

let orange = rgb 255 165 0
                   
let aff_of_color c = match c with
  | White -> white | Yellow -> yellow | Green -> green
  | Red -> red | Blue -> blue | Orange -> orange

let convert_square n = n mod 3,n/3
              
let square n n2 c =
  let x,y = convert_square n in
  let yg = if n2=0 then 3*gblock
           else if n2<5 then 2*gblock
           else gblock in 
  let xg = if n2=0 || n2=5 || n2=2 then gblock
           else if n2=1 then 0
           else if n2 = 3 then 2*gblock
           else 3*gblock in
  set_color (aff_of_color c);
  fill_rect (xg+x*block) (yg-y*block-block) block block

type dir = U | L | D | R | N

let lineE e epl = match e with
  | U -> (0,-epl) | L -> (epl,0) | D -> (0,epl) 
  | R -> (-epl,0) | N -> (0,0)
                       
let lineA xd yd xa ya e =
  set_color black;
  moveto (xd*block) (yd*block);
  lineto (xa*block) (ya*block);
  let offset1,offset2 = lineE e 1 in
  moveto (xd*block+offset1) (yd*block+offset2);
  lineto (xa*block+offset1) (ya*block+offset2)

let cord_of_int n = match n with
  | 0 -> (3,9) | 1 -> (0,6) | 2 -> (3,6)
  | 3 -> (6,6) | 4 -> (9,6) | 5 -> (3,3)
  | _ -> failwith "cord_of_int"
  
let lineS x y =
  [(x,y,x+3,y,U);
   (x,y-1,x+3,y-1,N);
   (x,y-2,x+3,y-2,N);
   (x,y-3,x+3,y-3,D);  
   (x,y,x,y-3,L);
   (x+1,y,x+1,y-3,N);
   (x+2,y,x+2,y-3,N);
   (x+3,y,x+3,y-3,R)]
           
let line () =
  List.iter (fun (xd,yd,xa,ya,e) -> lineA xd yd xa ya e) (List.concat (List.map (fun a -> let a,b = (cord_of_int a) in lineS a b ) ([0;1;2;3;4;5])));
  List.iter (fun (xd,yd,xa,ya,e) -> lineA xd yd xa ya e) ([(3,9,3,0,R);(6,9,6,0,L);(0,6,12,6,D);(0,3,12,3,U)])

let draw_all cube =
  Array.iteri (fun i x -> square (i mod 9) (i/9) x) cube;
  line ()             

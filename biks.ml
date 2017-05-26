type color =
  | White
  | Yellow
  | Green
  | Red
  | Blue
  | Orange

type turntype =
  | U
  | R
  | L
  | D
  | B
  | F

let int_of_color c = match c with
  | White -> 4
  | Yellow -> 2
  | Green -> 3
  | Red -> 5
  | Blue -> 1
  | Orange -> 0

(* For now, there are not x/y/z moves*)
               
let int_of_turntype t = match t with
  | U -> 2
  | R -> 3
  | L -> 1
  | D -> 4
  | B -> 0
  | F -> 5

let color_of_int n = match n with
  | 4 -> White
  | 2 -> Yellow
  | 3 -> Green
  | 5 -> Red
  | 1 -> Blue
  | 0 -> Orange
  | _ -> failwith "Not a color"
               
module type CUBE = sig 
  type t
  val construct : unit -> t
  val turn : t -> turntype -> t
end

let cord n shift = n*9 + shift
      
let adjacent_color n = match n with
    
  | 4 -> [[cord 1 0; cord 5 6; cord 3 8; cord 0 0];
        [cord 1 6; cord 5 8; cord 3 2; cord 0 2];
        [cord 1 3; cord 5 7; cord 3 5; cord 0 1]]
          
  | 2 -> [[cord 1 8; cord 0 6; cord 3 0; cord 5 2];
        [cord 1 2; cord 0 8; cord 3 6; cord 5 0];
        [cord 1 5; cord 0 7; cord 3 3; cord 5 1]]
          
  | 3 -> [[cord 2 8; cord 0 8; cord 4 0; cord 5 8];
        [cord 2 2; cord 0 2; cord 4 6; cord 5 2];
        [cord 2 5; cord 0 5; cord 4 3; cord 5 5]]
          
  | 5 -> [[cord 1 6; cord 2 6; cord 3 6; cord 4 6];
        [cord 1 8; cord 2 8; cord 3 8; cord 4 8];
        [cord 1 7; cord 2 7; cord 3 7; cord 4 7]]
          
  | 1 -> [[cord 4 8; cord 0 0; cord 2 0; cord 5 0];
        [cord 4 2; cord 0 6; cord 2 6; cord 5 6];
        [cord 4 5; cord 0 3; cord 2 3; cord 5 3]]
          
  | 0 -> [[cord 1 8; cord 4 2; cord 3 2; cord 2 2];
        [cord 1 2; cord 4 0; cord 3 0; cord 2 0];
        [cord 1 5; cord 4 1; cord 3 1; cord 2 1]]
          
  | _ -> failwith "Not a color"
                              
                     
module Cube = struct
  type t = Array of color

  let construct () = Array.init 54 (fun x -> color_of_int (x/9))

  let cycle cub l = match l with
    | [] -> failwith "empty cycle "
    | x::q ->
       begin
         let temp = x in
         let rec aux_cycle l = match l with
           | [] -> failwith "aux_cycle error"
           | [y] -> cub.(y) <- temp
           | u1::u2::q -> cub.(u1) <- cub.(u2); aux_cycle q
         in aux_cycle q
       end
                                    
  let turn cub k =
    
    let face_turn i = 
      cycle cub [i;i+6;i+8;i+2]; (* corners*)
      cycle cub [i+1;i+3;i+7;i+5] (* edges *)
    in

    let other_turn i =
      List.iter (fun l -> cycle cub l) (adjacent_color i)
    in
    
    let i = int_of_turntype k in
    face_turn i;
    other_turn i    
end

let cube = Cube.construct ()
                          
let _ = Array.iter (fun x -> Printf.printf "%d " (int_of_color x)) cube;
        Printf.printf "\n"

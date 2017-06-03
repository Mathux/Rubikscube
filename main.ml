open Graphics
open Biks
open Draw
open Draw3d
       
let rec play drawf =
  graphics_open();
  drawf (Cube.draw ());
  let e = wait_next_event [Key_pressed] in
  let x = e.mouse_x and y = e.mouse_y in
  set_color black;
  fill_rect x y 50 50;
  if e.keypressed then begin
	  match e.key with
      | '\027' -> close_graph ();
      | 'u' -> Cube.turn [Biks.U]; play drawf
      | 'r' -> Cube.turn [Biks.R]; play drawf
      | 'l' -> Cube.turn [Biks.L]; play drawf
      | 'd' -> Cube.turn [Biks.D]; play drawf
      | 'b' -> Cube.turn [Biks.B]; play drawf
      | 'f' -> Cube.turn [Biks.F]; play drawf
      | 'U' -> Cube.turn [Biks.U;Biks.U;Biks.U]; play drawf
      | 'R' -> Cube.turn [Biks.R;Biks.R;Biks.R]; play drawf
      | 'L' -> Cube.turn [Biks.L;Biks.L;Biks.L]; play drawf
      | 'D' -> Cube.turn [Biks.D;Biks.D;Biks.D]; play drawf
      | 'B' -> Cube.turn [Biks.B;Biks.B;Biks.B]; play drawf
      | 'F' -> Cube.turn [Biks.F;Biks.F;Biks.F]; play drawf
      | 'm' -> Cube.turn (Array.to_list (Array.init 50 (fun i -> turntype_of_int(Random.int 6)))); play drawf
      | ' ' -> Cube.restart (); play drawf                                    
      | ll -> try Cube.turn (Array.to_list (Array.init (let nb = int_of_string (Printf.sprintf "%c" ll) in if (nb < 10 && nb>=0) then nb else failwith "invalid number") (fun i -> turntype_of_int(Random.int 6)))); play drawf with
             | _ -> play drawf;
    end
  else play drawf;
  close_graph ()


              
let explain () =
  Format.printf "f3d for 3d, nothing of something for 2d@."
                
let _ = try
    (match Sys.argv.(1) with
     | "3d" -> play draw_all3d
     | "d" -> play (fun x -> draw_all x ; draw_all3d x)
     | _ -> explain (); play draw_all
    )
  with
  | _ -> play draw_all


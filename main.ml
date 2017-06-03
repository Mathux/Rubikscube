open Graphics
open Biks
open Draw
open Draw3d
       
let rec f () =
  graphics_open();
  draw_all (Cube.draw ());
  let e = wait_next_event [Key_pressed] in
  let x = e.mouse_x and y = e.mouse_y in
  set_color black;
  fill_rect x y 50 50;
  if e.keypressed then begin
	  match e.key with
      | '\027' -> close_graph ();
      | 'u' -> Cube.turn [Biks.U]; f ()
      | 'r' -> Cube.turn [Biks.R]; f ()
      | 'l' -> Cube.turn [Biks.L]; f ()
      | 'd' -> Cube.turn [Biks.D]; f ()
      | 'b' -> Cube.turn [Biks.B]; f ()
      | 'f' -> Cube.turn [Biks.F]; f ()
      | 'U' -> Cube.turn [Biks.U;Biks.U;Biks.U]; f ()
      | 'R' -> Cube.turn [Biks.R;Biks.R;Biks.R]; f ()
      | 'L' -> Cube.turn [Biks.L;Biks.L;Biks.L]; f ()
      | 'D' -> Cube.turn [Biks.D;Biks.D;Biks.D]; f ()
      | 'B' -> Cube.turn [Biks.B;Biks.B;Biks.B]; f ()
      | 'F' -> Cube.turn [Biks.F;Biks.F;Biks.F]; f ()
      | 'm' -> Cube.turn (Array.to_list (Array.init 50 (fun i -> turntype_of_int(Random.int 6)))); f ()
      | ' ' -> Cube.restart (); f ()                                    
      | ll -> try Cube.turn (Array.to_list (Array.init (let nb = int_of_string (Printf.sprintf "%c" ll) in if (nb < 10 && nb>=0) then nb else failwith "invalid number") (fun i -> turntype_of_int(Random.int 6)))); f () with
             | _ -> f ();
    end
  else f ();
  close_graph ()


let rec f3d () =
  graphics_open();
  draw_all3d (Cube.draw ());
  let e = wait_next_event [Key_pressed] in
  let x = e.mouse_x and y = e.mouse_y in
  set_color black;
  fill_rect x y 50 50;
  if e.keypressed then begin
	  match e.key with
      | '\027' -> close_graph ();
      | 'u' -> Cube.turn [Biks.U]; f3d ()
      | 'r' -> Cube.turn [Biks.R]; f3d ()
      | 'l' -> Cube.turn [Biks.L]; f3d ()
      | 'd' -> Cube.turn [Biks.D]; f3d ()
      | 'b' -> Cube.turn [Biks.B]; f3d ()
      | 'f' -> Cube.turn [Biks.F]; f3d ()
      | 'U' -> Cube.turn [Biks.U;Biks.U;Biks.U]; f3d ()
      | 'R' -> Cube.turn [Biks.R;Biks.R;Biks.R]; f3d ()
      | 'L' -> Cube.turn [Biks.L;Biks.L;Biks.L]; f3d ()
      | 'D' -> Cube.turn [Biks.D;Biks.D;Biks.D]; f3d ()
      | 'B' -> Cube.turn [Biks.B;Biks.B;Biks.B]; f3d ()
      | 'F' -> Cube.turn [Biks.F;Biks.F;Biks.F]; f3d ()
      | 'm' -> Cube.turn (Array.to_list (Array.init 50 (fun i -> turntype_of_int(Random.int 6)))); f3d ()
      | ' ' -> Cube.restart (); f3d ()                                    
      | ll -> try Cube.turn (Array.to_list (Array.init (let nb = int_of_string (Printf.sprintf "%c" ll) in if (nb < 10 && nb>=0) then nb else failwith "invalid number") (fun i -> turntype_of_int(Random.int 6)))); f3d () with
             | _ -> f3d ();
    end
  else f3d ();
  close_graph ()

              
let explain () =
  Format.printf "f3d for 3d, nothing of something for 2d@."
                
let _ = try
    (match Sys.argv.(1) with
     | "3d" -> f3d ()
     | _ -> explain (); f ()
    )
  with
  | _ -> f ()                    
          


open Graphics

let rec f () =
  let cube = Biks.cube in
  Draw.graphics_open();
  let e = wait_next_event [Key_pressed] in
  if e.keypressed then begin
	  match e.key with
      | 'u' -> Biks.Cube.turn Biks.cube Biks.U; f ()
      | _ -> f ();
    end
  else f ();
  close_graph ()

let _ = f ()

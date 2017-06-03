open Graphics
open Biks
open Draw

let square3d n n2 c =
      let x,y = convert_square n in
      set_color (aff_of_color c);
      match n2 with
      | 2 -> let arr = [| (x*block+gblock+(3-y)*block/2 - block/2,2*gblock-gblock/2-y*block/2-block/2);
                         (x*block+gblock+(3-y)*block/2 + block/2,2*gblock-gblock/2-y*block/2-block/2);
                         (x*block+gblock+(3-y)*block/2 + block,2*gblock-gblock/2-y*block/2);
                         (x*block+gblock+(3-y)*block/2 ,2*gblock-gblock/2-y*block/2) |] in
            fill_poly arr; set_color black; draw_poly arr
                       
      | 3 -> let arr = [| ((2-y)*block/2+2*gblock,gblock-x*block+(2-y)*block/2-block);
                         ((2-y)*block/2+2*gblock+block/2,gblock-x*block+(2-y)*block/2-block/2);
                         ((2-y)*block/2+2*gblock+block/2,gblock-x*block+(2-y)*block/2+block/2);
                         ((2-y)*block/2+2*gblock,gblock-x*block+(2-y)*block/2) |]in
            fill_poly arr; set_color black; draw_poly arr
                                                      
      | 5 -> let arr = [|(gblock+x*block,gblock-y*block-block);
                        (gblock+x*block+block,gblock-y*block-block);
                        (gblock+x*block+block,gblock-y*block);
                        (gblock+x*block,gblock-y*block)|]in
                        fill_poly arr; set_color black; draw_poly arr
      | _ -> ()
              
let draw_all3d cube =
  Array.iteri (fun i x -> square3d (i mod 9) (i/9) x) cube;

open Graphics
open Biks
open Draw

let offx = block and offy = 2*block

let offx2 = offx + 2*gblock and offy2 = offy + block
                
  
let square3d n n2 c =
      let x,y = convert_square n in
      set_color (aff_of_color c);
      match n2 with
      | 2 -> let arr = [| (offx+x*block+(3-y)*block/2 - block/2,offy+2*gblock-gblock/2-y*block/2-block/2);
                         (offx+x*block+(3-y)*block/2 + block/2,offy+2*gblock-gblock/2-y*block/2-block/2);
                         (offx+x*block+(3-y)*block/2 + block,offy+2*gblock-gblock/2-y*block/2);
                         (offx+x*block+(3-y)*block/2 ,offy+2*gblock-gblock/2-y*block/2) |] in
            fill_poly arr; set_color black; draw_poly arr
                       
      | 3 -> let arr = [| (offx+(2-y)*block/2+gblock,offy+gblock-x*block+(2-y)*block/2-block);
                         (offx+(2-y)*block/2+gblock+block/2,offy+gblock-x*block+(2-y)*block/2-block/2);
                         (offx+(2-y)*block/2+gblock+block/2,offy+gblock-x*block+(2-y)*block/2+block/2);
                         (offx+(2-y)*block/2+gblock,offy+gblock-x*block+(2-y)*block/2) |]in
            fill_poly arr; set_color black; draw_poly arr
                                                      
      | 5 -> let arr = [| (offx+x*block,offy+gblock-y*block-block);
                         (offx+x*block+block,offy+gblock-y*block-block);
                         (offx+x*block+block,offy+gblock-y*block);
                         (offx+x*block,offy+gblock-y*block)|]in
            fill_poly arr; set_color black; draw_poly arr

      | 0 -> let arr = [| (offx2+(2-x)*block,offy2+gblock-(2-y)*block-block);
                         (offx2+(2-x)*block+block,offy2+gblock-(2-y)*block-block);
                         (offx2+(2-x)*block+block,offy2+gblock-(2-y)*block);
                         (offx2+(2-x)*block,offy2+gblock-(2-y)*block)|]in
            fill_poly arr; set_color black; draw_poly arr

      | 1 -> let arr = [| (offx2+y*block/2+gblock,offy2+gblock-(2-x)*block-y*block/2-block);
                         (offx2+y*block/2+gblock+block/2,offy2+gblock-(2-x)*block-y*block/2-block/2-block);
                         (offx2+y*block/2+gblock+block/2,offy2+gblock-(2-x)*block-y*block/2+block/2-block);
                         (offx2+y*block/2+gblock,offy2+gblock-(2-x)*block-y*block/2) |]in
            fill_poly arr; set_color black; draw_poly arr

      | 4 -> let arr = [| (offx2+x*block-(3-y)*block/2+3*block,offy2+(2-y)*block/2-block/2-block);
                         (offx2+x*block-(3-y)*block/2+2*block,offy2+(2-y)*block/2-block/2-block);
                         (offx2+x*block-(3-y)*block/2-block/2+2*block,offy2+(2-y)*block/2-block);
                         (offx2+x*block-(3-y)*block/2 - block/2+3*block,offy2+(2-y)*block/2-block) |] in
            fill_poly arr; set_color black; draw_poly arr
                                                      
      | _ -> ()
              
let draw_all3d cube =
  Array.iteri (fun i x -> square3d (i mod 9) (i/9) x) cube;

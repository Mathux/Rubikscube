type color = White | Yellow | Green | Red | Blue | Orange

type turntype = U | R | L | D | B | F

val int_of_color : color -> int
               
val int_of_turntype : turntype -> int

val turntype_of_int : int -> turntype

val color_of_int : int -> color
                           
module type CUBE = sig
  type t
  val turn : turntype list -> unit
  val restart : unit -> unit
  val draw : unit -> color array
end
                                                
module Cube : CUBE

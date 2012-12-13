(* particles.ml
   A particle engine.
*)

open Mass

class particle pos vel lifetime =
object (self) 
  inherit mass 1. pos vel
  val mutable life = lifetime

  method calc (t:float) =
    life <- life -. t;
    if life < 0. then 
      self#setAlive false
    else  ( 
	accel#set 0. 0. 0.;
      )
      
end
  

(* mass.ml
   Any object with mass.
   ie, anything.

   Simon Heath
*)

open Vector


class mass m p v =
object (self)
  val mutable mass : float = m
  val mutable pos : vector = p
  val mutable vel : vector = v

  val mutable accel : vector = new vector 0. 0. 0.

  val mutable alive = true
  val mutable radius = m ** 0.3

  method mass = mass
  method pos = pos
  method vel = vel
  method radius = radius

  method setMass m = mass <- m
  method setPos p = pos <- p
  method setVel v = vel <- v
  method setRadius r = radius <- r
  method addAccel a = accel <- accel#add a

  method distanceFrom (m : mass) =
    let distvec = pos#sub m#pos in
      distvec#magnitude

  method isColliding m =
    let d = self#distanceFrom m in
      d < (radius +. m#radius)

  (* I think we find the distance vector between the two points,
     get the unit vector, then multiply it by the magnitude of the
     acceleration.
     That's the acceleration vector, and we just add it to the velocity.
  *)
  method doGravity m = 
    let r = self#distanceFrom m in
    let accelf = (-. Globals.g *. mass *. m#mass) /. (r *. r) in
    let distvec = pos#sub m#pos in
    let forcevec =  (distvec#unitVector#mul accelf) in
      self#addAccel (forcevec#div mass)#invert;
      m#addAccel (forcevec#div m#mass);

  method calc (t : float) =
    (* p(t) = p0 + v*t + 0.5*a*t^2 *)
    pos <- (pos#add (vel#mul t))#add (accel#mul (0.5 *. t *. t));
    vel <- vel#add (accel#mul t);
    accel#set 0. 0. 0.;

  method moveTo v =
    pos <- v

  method isAlive = alive

  method setAlive s =
    alive <- s

  method impact (m : mass) =
    if m#mass > mass then
      self#setAlive false
    else
      m#setAlive false;

  method velocity = 
     vel#magnitude

  method momentum =
     self#velocity *. mass

  method toString =
    Printf.sprintf "Mass: %0.3e, Pos: %s, Vel: %s" mass pos#toString vel#toString;

  method copy =
    new mass mass pos#copy vel#copy

end;;

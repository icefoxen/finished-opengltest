(* vector.ml
   A class for a "vector", which is something that represents movement.

   Simon Heath
*)


class vector a b c =
object (self)
  val mutable x = a
  val mutable y = b
  val mutable z = c
		    
  (* Get the direction or magnitude of the vector *)
  method unitVector = 
    self#div self#magnitude
	    
  method magnitude = sqrt ((x *. x) +. (y *. y) +. (z *. z))
		   
  method set x' y' z' =
    x <- x';
    y <- y';
    z <- z';

  method setV (v:vector) =
    x <- v#x;
    y <- v#y;
    z <- v#z;

  method setX x' =
    x <- x'

  method setY y' =
    y <- y'

  method setZ z' =
    z <- z'

  method x = x
  method y = y
  method z = z

  (* Adds another vector to this one --acceleration, for instance *)
  method add (v : vector) =
    new vector (x +. v#x) (y +. v#y) (z +. v#z)

  method sub (v : vector) =
    new vector (x +. v#x) (y +. v#y) (z +. v#z)

  method mul f =
    new vector (x *. f) (y *. f) (z *. f)

  method div f =
    new vector (x /. f) (y /. f) (z /. f)

  method cross (v : vector) =
    let a = v#x
    and b = v#y
    and c = v#z in
    new vector (b*.z -. c*.y) (-.a*.z -. c*.x) (a*.y -. b*.x)

  method dot (v : vector) =
    (x *. v#x) +. (y *. v#y) +. (z *. v#z)

  method invert =
    self#mul (-1.0)

  method toString =
    Printf.sprintf "<%0.1e %.1e, %.1e>" x y z

  method copy = new vector x y z    

end;;


let zeroVector = new vector 0. 0. 0.;;

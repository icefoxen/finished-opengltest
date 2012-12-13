(* camera.ml
   A camera!
   It stays focussed on an object, and rotates and moves around it
   using polar coordinates.
*)

open Vector
open Mass


class camera focusobj =
object (self)

  val mutable x = 0.
  val mutable y = 0.
  val mutable z = 0.
  val mutable facing = new vector 0. 0. 1.

  method getPos = new vector x y z
  method getFacing = facing

  method setFacing v = facing <- v;

  method setXFacing x = facing#setX x
  method setYFacing y = facing#setY y
  method setZFacing z = facing#setZ z

  method addXFacing x = self#setXFacing (x +. facing#x);
  method addYFacing y = self#setYFacing (y +. facing#y);
  method addZFacing z = self#setZFacing (z +. facing#z);

  method moveTo x' y' z' = 
    x <- x';
    y <- y';
    z <- z';

  method moveToV (v : vector) = 
    x <- v#x;
    y <- v#y;
    z <- v#z;

  method addX x0 = x <- x +. x0
  method addY y0 = y <- y +. y0
  method addZ z0 = z <- z +. z0


end;;

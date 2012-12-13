(* driver.ml
   Makes things go.
*)

open Vector
open Mass
open Particles
open Sdlkey



let buildSystem () =
    let sun = new mass 2.0e30 (new vector 0. 0. 0.) (new vector 0. 0. 0.)
    and mercury = new mass 3.3e23 (new vector 55e9 0. 0.) (new vector 0. 47000. 0.)
    and venus = new mass 4.9e24 (new vector 108e9 0. 0.) (new vector 0. 35000. 0.)
    and earth = new mass 6.0e24 (new vector 149.0e9 0. 0.) (new vector 0. 30000. 0.)
(*    and luna = new mass 1.0e22 (new vector 149.38e9 0. 0.) (new vector 0. 30000. 0.) *)
    and mars = new mass 6.4e23 (new vector 225e9 0. 0.) (new vector 0. 23000. 0.)
    and ceres = new mass 6.0e21 (new vector 400e9 0. 0.) (new vector 0. 18000. 0.)
    and jupiter = new mass 1.9e27 (new vector 780e9 0. 0.) (new vector 0. 13000. 0.)
    and saturn = new mass 5.7e26 (new vector 1425e9 0. 0.) (new vector 0. 9600. 0.)
    and uranus = new mass 8.7e25 (new vector 2900e9 0. 0.) (new vector 0. 6900. 0.)
    and neptune = new mass 1.0e26 (new vector 4500e9 0. 0.) (new vector 0. 5400. 0.)
    and pluto = new mass 1.3e22 (new vector 4400e9 0. 0.) (new vector 0. 6100. 0.)
    and eris = new mass 1.6e22 (new vector 5700e9 0. 0.) (new vector 0. 4100. 0.)

    and projectile = new mass 1e3 (new vector 150e9 0. 0.) (new vector 3e8 0. 0.)
      

    in
      [sun;mercury;venus;earth;mars;ceres;jupiter;saturn;uranus;neptune;pluto;eris;projectile];
;;



class driver =
object (self)

  val mutable masses : mass list = []
  val mutable particles : particle list = []
  val mutable lastMasses = []
  val mutable continue = true


  val mutable lastTick = 0
  val mutable thisTick = 0
  val mutable timeScale = 1e7

  method addMass m =
    masses <- m :: masses

  method addParticle p =
    particles <- p :: particles;

  method delMass m =
    masses <- List.filter (fun x -> x <> m) masses


  method doInput drawer =
    ignore (Sdlevent.poll ());
    if (is_key_pressed !Input.menu) or (is_key_pressed !Input.help) then
      self#stopGame;

    if is_key_pressed !Input.pause then
      self#pause;

    let c = drawer#getCamera in
    if is_key_pressed !Input.camerapx then
      c#addXFacing (0.1);
    if is_key_pressed !Input.cameranx then
      c#addXFacing (-0.1);

    if is_key_pressed !Input.camerapy then
      c#addYFacing (0.1);
    if is_key_pressed !Input.camerany then
      c#addYFacing (-0.1);

    if is_key_pressed !Input.camerapz then
      c#addZ (0.01);
    if is_key_pressed !Input.cameranz then
      c#addZ (-0.01);

  method stopGame =
    continue <- false;

  method pause =
    print_endline "Pausing, supposedly.";

  (* All time is in seconds *)
  (* This is a bit nastier than it should be, since each object has to have
     its gravity calculated against each other object once and only once.
  *)
  method doGravity =
    let rec calcGravFor itm = function
	[] -> ()
      | hd :: tl -> itm#doGravity hd; calcGravFor itm tl
    in
    let rec loop = function
	[] -> ()
      | hd :: tl -> calcGravFor hd tl; loop tl
    in
      loop masses

  method doCalc t =
    let makeTrailParticle mass =
      if (lastTick / 3000) < (thisTick / 3000) then
	self#addParticle 
	  (new Particles.particle mass#pos zeroVector 1e12)
    in

      List.iter (fun x -> x#calc t) masses; 
      List.iter (fun x -> x#calc t) particles;
      List.iter makeTrailParticle masses;
      


  method doImpact =
    let rec calcImpactFor itm = function
	[] -> ()
      | hd :: tl -> if itm#isColliding hd then (itm#impact hd; hd#impact itm);
	  calcImpactFor itm tl
    in
    let rec loop = function
	[] -> ()
      | hd :: tl -> calcImpactFor hd tl; loop tl
    in
      loop masses
	
  method doDeath =
    masses <- List.filter (fun x -> x#isAlive) masses;
    particles <- List.filter (fun x -> x#isAlive) particles;
    

  (* The order of these is important *)
  method calcMasses t =
    self#doGravity;
    self#doCalc t;
    self#doImpact;
    self#doDeath;



  method stop =
    continue <- false

  method print =
    List.iter (fun x -> print_endline x#toString) masses;


  method mainloop = 
      let d = new Drawing.drawer in
	masses <- buildSystem ();
	while continue do
	  lastTick <- thisTick;
	  thisTick <- Sdltimer.get_ticks ();
	  let dt = timeScale *. 
	    ((float_of_int (thisTick - lastTick)) /. 1000.) in
	    self#doInput d;
	    self#calcMasses dt;
	    d#draw masses particles;
(*	    print_endline d#getCamera#toString; *)
	done; 
	

end;;

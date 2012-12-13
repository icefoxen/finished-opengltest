open Driver;;
open Drawing;;


let _ = 

  Sdl.init [`EVERYTHING];


    let driver = new driver in
      driver#mainloop;

    Sdl.quit ();

;;

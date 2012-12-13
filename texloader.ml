(* texloader.ml
   Banana hammock.
   Basically, we use the SDL loader, and turn it into an OpenGL RAW
   format.
*)

open Sdlvideo;;

let iSize = 64;;

let hardwiredTexture () = 
  let image = GlPix.create `ubyte ~height: iSize ~width: iSize ~format: `rgb in
  let raw = GlPix.to_raw image in
    for i = 0 to iSize - 1 do
      for j = 0 to iSize - 1 do
	let itm = if Random.int 100 < 10 then [|255;255;255|]
	  else [|0;0;0|] in
	  Raw.sets raw ~pos: (3*(i*iSize+j)) itm;
      done;
      done;

      image
;;


let loadTexture filename =
  let surf = Sdlloader.load_image filename in
  let size = surface_info surf in
  let image = GlPix.create `ubyte ~height: size.h ~width: size.w
    ~format: `rgb in
  let raw = GlPix.to_raw image in
    (* Will these be right-side up and everything? *)
    lock surf;
    for i = 0 to size.h - 1 do
      for j = 0 to size.w - 1 do
	let r, g, b = get_pixel_color surf ~x: i ~y: j in
	  Raw.sets raw ~pos: (3*(i*size.w + j)) [|r;g;b|];
      done;
      done;
      unlock surf;
      image
;;

(* util.ml
   Basic utility junk and global vars.
   And the stuff the OCaml standard library should have, and dosen't.
   
   Simon Heath
*)

open Sdlvideo;;

(*
let screenx = ref 800;;
let screeny = ref 600;;
let logscreenx = ref 0.;;
let logscreeny = ref 0.;;
let logscreenw = ref 0.;;
let logscreenh = ref 0.;;
*)



let print_bool = function
    true -> print_string "true"
  | false -> print_string "false"
;;

let error x y = 
  Printf.eprintf x y;
  exit 1;
;;


let pi = acos (-1.0);;
let d2r x = x *. (pi /. 180.);;
let r2d x = x *. (180. /. pi);;
let absf x =
  if x < 0. then
    -.x
  else
    x
;;

let absmod x y = 
  let n = x mod y in
    abs n
;;

let fabs x =
  if x < 0. then
    -. x
  else
    x
;;


let incf x =
  x := !x +. 1.;;

let decf x = 
  x := !x -. 1.;;

let removeNth lst n =
  let rec loop n lst = 
    if n = 0 then
      List.tl lst
    else 
      (List.hd lst) :: (loop (n - 1) (List.tl lst))
  in
    if List.length lst > n then
      raise (Failure "removeNth: list too long")
    else
      loop n lst
;;


let square x =
  x *. x
;;

(* Return true if a is equal to b within the given delta *)
let within a b delta =
  absf (a -. b) > delta
;;



(* Why can't I just chop the first or last x characters from a string,
   easily?
*)
let chop_left s i =
  let ns = String.create ((String.length s) - i) in
    for x = i to ((String.length s) - 1) do
      ns.[x - i] <- s.[x]
    done;
    ns
;;

let chop_right s i =
  String.sub s 0 ((String.length s) - i)
;;

(* Trims whitespace from the beginning and end of a string *)
let trim s =
  let stptr = ref 0 
  and endptr = ref ((String.length s) - 1)
  in
    while s.[!stptr] = ' ' || s.[!stptr] = '\t' || s.[!stptr] = '\n' do
      incr stptr;
    done;
    while s.[!endptr] = ' ' || s.[!endptr] = '\t' || s.[!endptr] = '\n' do
      decr endptr;
    done;
    let strlen = !endptr - !stptr + 1 in
      String.sub s !stptr strlen
;;


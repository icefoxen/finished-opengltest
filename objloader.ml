(* A set of loader functions for Wavefront .obj files 
   Doesn't handle materials yet.
   Only loads triangles, too.
   In fact, it ignores everything except the actual vertices
   and faces.

   Someday, turning this into a display list would be nice.
*)

(* OCaml array-of-float's are optimized better by the compiler *)
type vertex = float array;;

type vertexNormal = float array;;

(* Each face shall just be a list of offsets into the vertex array 
   A face can be arbitrarily big, I suppose.
*)
type face = int array;;

type mesh = {
    m_vertices : vertex array;
    m_normals : vertexNormal array;
    m_faces : face array;
(*    m_material : unit; *)
  };;


let parseVertex line =
  try
    Scanf.sscanf line "v %f %f %f" 
      (fun x y z -> [|x;y;z|])
  with
      Scanf.Scan_failure _ ->
	raise (Failure ("parseVertex: scanf choked on:\n" ^ line));
;;


(* Only loads triangles! *)
let parseFace line =
  try
    Scanf.sscanf line "f %d//%d %d//%d %d//%d"
      (fun x _ y _ z _ ->
	[|x; y; z|])
  with
      Scanf.Scan_failure _ ->
	raise (Failure ("parseFace: scanf choked on " ^ line));
;;

let parseNormal line =
  try
    Scanf.sscanf line "vn %f %f %f"
      (fun x y z ->
	[|x; y; z|])
  with
      Scanf.Scan_failure _ ->
	raise (Failure ("parseNormal: scanf choked on " ^ line));
;;



let loadObj filename =
  let f = open_in filename in
  let rec loop vertices normals faces =
    try
      let line = input_line f in
	if line.[0] = 'v' && line.[1] = ' ' then
	  let v = parseVertex line in
	    loop (v :: vertices) normals faces

	else if line.[0] = 'v' && line.[1] = 'n' then
	  let n = parseNormal line in
	    loop vertices (n :: normals) faces
	else if line.[0] = 'f' && line.[1] = ' ' then
	  let f = parseFace line in
	    loop vertices normals (f :: faces)

	else
	  loop vertices normals faces
    with
	End_of_file ->
	  {
	    m_vertices = (Array.of_list vertices); 
	    m_normals = (Array.of_list normals);
	    m_faces = (Array.of_list faces);
	  }
  in
    loop [] [] []
;;


let drawFace vertices normals face =

  GlDraw.begins `triangles;
  for i = 0 to (Array.length face) - 1 do
    let v = vertices.( face.(i) - 1 ) 
    and n = normals.( face.(i) - 1 ) in
      GlDraw.normal ~x: n.(0) ~y: n.(1) ~z: n.(2) ();
      GlDraw.vertex3 (v.(0), v.(1), v.(2));
  done;
  GlDraw.ends ();
;;


let drawMesh mesh =
  let v = mesh.m_vertices
  and n = mesh.m_normals
  and f = mesh.m_faces in

    for i = 0 to (Array.length f) - 1 do
      drawFace v n f.(i)
    done

;;


let mesh = loadObj "torus.obj";;

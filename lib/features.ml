open Matrix

type point = float * float * float * float;;

let epsilon = 0.0000001

let make_point (x: float) (y: float) (z: float) : point = 
  (x, y, z, 1.);;

let make_vector (x: float) (y: float) (z: float) : point = 
  (x, y, z, 0.);;

let is_point (x: point) : bool = 
  let _, _, _, p = x in 
  p = 1.;;

let is_vector (x: point) : bool = 
  let _, _, _, p = x in 
  p = 0.;;

let add (a: point) (b: point) : point = 
  let x, y, z, p = a in 
  let x2, y2, z2, p2 = b in 
  if p+.p2 > 1.5 then failwith "attempted to add 2 points" 
  else (x+.x2, y+.y2, z+.z2, p+.p2);;
  
let sub (a: point) (b: point) : point = 
  let x, y, z, p = a in 
  let x2, y2, z2, p2 = b in 
  if p-.p2 < (-0.5) then failwith "attempted to subtract point from vector"
  else (x-.x2, y-.y2, z-.z2, p-.p2);;

let neg (a: point) : point = 
  let x, y, z, p = a in 
  (0.-.x, 0.-.y, 0.-.z, p);;

let mult (a: point) (s: float) : point = 
  let x, y, z, p = a in 
  (x*.s, y*.s, z*.s, p);;

let div (a: point) (s: float) : point = 
  if s = 0. then failwith "divide by zero" else
  mult a (1. /. s);;

let mag (a: point) : float = 
  if is_point a then failwith "attempted to take magnitude of a point" 
  else let x, y, z, _ = a in 
  sqrt (x*.x +. y*.y +. z*.z);;

let norm (a: point) : point = 
  if is_point a then failwith "attempted ot take normal of a point"
  else div a (mag a);;

let dot (a: point) (b: point) : float = 
  if (is_point a) || (is_point b) then failwith "attempted to take dot product of points" 
  else 
    let x, y, z, _ = a in 
    let x2, y2, z2, _ = b in 
    x*.x2 +. y*.y2 +. z*.z2;;

let cross (a: point) (b: point) : point = 
  if (is_point a) || (is_point b) then failwith "attempted to take cross product of points"
  else 
    let x, y, z, _ = a in 
    let x2, y2, z2, _ = b in 
    make_vector (y*.z2 -. z*.y2) (z*.x2 -. x*.z2) (x*.y2 -. y*.x2);;

let matrix_of_point (a: point) : Matrix.matrix = 
  let a, b, c, d = a in Matrix.matrix_of [[a];[b];[c];[d]]

let point_of_matrix (a: Matrix.matrix) : point = 
  match Matrix.lists_of (Matrix.transpose a) with 
  | [a::b::c::d::[]] -> (a, b, c, d)
  | _ -> failwith "matrix could not be converted to a point"

let string_of_point (a: point) : string = 
  let a, b, c, d = a in 
  "(" ^ string_of_float a ^ ", " 
  ^ string_of_float b ^ ", "
  ^ string_of_float c ^ ", " 
  ^ string_of_float d ^ ")"

let transform_point (a: point) (m: matrix) : point = 
  let a = matrix_of_point a in 
  let a = mult_m m a in 
  point_of_matrix a

let reflect (a: point) (n: point) : point = 
  if is_point a || is_point n then failwith "trying to reflect points instead of vectors" else
  sub a (mult n (2. *. (dot a n)))
type matrix = float array array

let matrix_epsilon = 0.0000001

let equal_m (a: matrix) (b: matrix) : bool = 
  let bools = Array.map2 
  (fun rowa rowb ->
    Array.map2 (fun item1 item2 -> abs_float (item1 -. item2) < matrix_epsilon)
    rowa rowb)
  a b in 
  let l = Array.map (fun row -> Array.fold_left (&&) true row) bools in 
  Array.fold_left (&&) true l

let m_of (a: matrix) : int = 
  Array.length a

let n_of (a: matrix) : int = 
  Array.length a.(0)

let empty_matrix (rows: int) (cols: int) : matrix = 
  Array.make_matrix rows cols 0.

let matrix_of (l: float list list) : matrix = 
  let l = List.map Array.of_list l in Array.of_list l

let identity_of (n: int) : matrix = 
  let zeroes = empty_matrix n n in 
  Array.mapi (fun index row -> let () = row.(index) <- 1. in row) zeroes

let lists_of (a: matrix) : float list list = 
  let a = Array.map Array.to_list a in Array.to_list a

let verify_not_ragged (m : matrix) : bool = 
  let _, proper = Array.fold_left 
  (fun (prev, ok) row -> if Array.length row = prev then (Array.length row, ok) else (0, false))
  (Array.length m.(0), true) m
  in proper

let rec transpose_helper (a: float list list) : float list list =
  match a with 
  | [] -> []
  | []::xss -> transpose_helper xss
  | (x::xs)::xss -> 
    (x::List.map List.hd xss)::transpose_helper (xs::List.map List.tl xss)

let transpose (a: matrix) : matrix = 
  matrix_of (transpose_helper (lists_of a))

let dot_product (a: float array) (b: float array) : float = 
  Array.fold_left (fun a x -> a+.x) 0. (Array.map2 (fun x y -> x*.y) a b)

let mult_m (a: matrix) (b: matrix) : matrix = 
  let b = transpose b in 
  Array.map (fun row -> Array.map (dot_product row) b) a

let remove (a: 'a array) (n: int) : 'a array = 
  let l = Array.length a in 
  let first = Array.sub a 0 n in 
  let second = Array.sub a (n+1) (l-n-1) in 
  Array.append first second

let submatrix (a: matrix) (row: int) (col: int) : matrix = 
  let a = remove a row in 
  let a = transpose a in 
  let a = remove a col in 
  transpose a

let det_2 (a: matrix) : float = 
  a.(0).(0) *. a.(1).(1) -. a.(0).(1) *. a.(1).(0)

let rec det (a: matrix) : float = 
  if m_of a = 1 && n_of a = 1 then a.(0).(0) else
  if m_of a = 2 && n_of a = 2 then det_2 a else 
    let semis = Array.mapi (fun i row -> (cofactor a i 0) *. row.(0)) a in 
    Array.fold_left (fun a x -> a+.x) 0. semis

and minor (a: matrix) (row: int) (col: int) : float = 
  det (submatrix a row col)

and cofactor (a: matrix) (row: int) (col: int) : float =
  let m = minor a row col in 
  if (row+col) mod 2 = 1 then -.m else m

let is_invertible (a: matrix) : bool = 
  abs_float (0. -. det a) > matrix_epsilon

let inverse (a: matrix) : matrix = 
  let d = det a in 
  let cofactor_matrix = Array.mapi (fun i row -> Array.mapi (fun j _ -> (cofactor a i j) /. d) row) a in 
  transpose cofactor_matrix

let apply_ms (ms: matrix list) (x: matrix) : matrix = 
  List.fold_right (fun m a -> mult_m m a) ms x

let combine (ms: matrix list) : matrix = 
  List.fold_right (fun m a -> mult_m m a) ms (identity_of 4)

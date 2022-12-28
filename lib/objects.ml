open Rays
open Features
open Matrix
open Materials

type shape = Sphere | Plane
type thing = { 
  shape: shape;
  transformation: matrix;
  material: material;
  }
type intersection = {
  t: float;
  o: thing
  }

let make_sphere ?(transformation = identity_of 4) ?(material = make_material ()) (): thing = 
  {shape=Sphere; transformation=transformation; material=material}

let make_plane ?(transformation = identity_of 4) ?(material = make_material ()) (): thing = 
  {shape=Plane; transformation=transformation; material=material}

let intersect (r: ray) (tng: thing) : intersection list = 
  let r = transform_ray r (inverse tng.transformation) in 
  let org, dir = r in 
  match tng.shape with 
  | Sphere ->
    let sphere_to_ray = sub org (make_point 0. 0. 0.) in 
    let a = dot dir dir in 
    let b = 2. *. (dot dir sphere_to_ray) in 
    let c = (dot sphere_to_ray sphere_to_ray) -. 1. in 
    let dscr = b *. b -. 4. *. a *. c in 
    if dscr < 0. then [] else 
      let t1 = (-.b -. (sqrt dscr)) /. (2. *. a) in 
      let t2 = (-.b +. (sqrt dscr)) /. (2. *. a) in 
      [{t=t1; o=tng}; {t=t2; o=tng}]
  | Plane ->
    let _, dy, _, _ = dir in
    let _, oy, _, _ = org in 
    if abs_float dy < epsilon then 
      [] 
    else 
      [{t=(-.oy)/.dy; o=tng}]
      

let rec hit (is: intersection list) : intersection option = 
  match is with 
  | [] -> None 
  | i::is -> let h = hit is in 
    let t = i.t in 
    match h with 
    | None -> if t < 0. then None else Some(i)
    | Some (h) -> if t < 0. then Some (h) else 
      let t2 = h.t in 
      if t2 < t then Some(h) 
      else Some(i)
  
let normal_at (tng: thing) (p: point) : point = 
  match tng.shape with 
  | Sphere -> 
    let obj_point = transform_point p (inverse tng.transformation) in 
    let obj_normal = sub obj_point (make_point 0. 0. 0.) in 
    let wrld_normal = transform_point obj_normal (transpose (inverse tng.transformation)) in 
    let a, b, c, _ = wrld_normal in let wrld_normal = make_vector a b c in 
    norm wrld_normal
  | Plane ->
    let obj_normal = make_vector 0. 1. 0. in 
    let wrld_normal = transform_point obj_normal (transpose (inverse tng.transformation)) in 
    let a, b, c, _ = wrld_normal in let wrld_normal = make_vector a b c in 
    norm wrld_normal



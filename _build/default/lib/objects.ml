open Rays
open Features
open Matrix
open Materials

type shape = Sphere
type thing = shape * matrix * material
type intersection = {t: float; o: thing}

let make_sphere (m: matrix option) (mt: material option): thing = 
  let m = match m with Some(m) -> m | None -> identity_of 4 in 
  let mt = match mt with Some(mt) -> mt | None -> default_material in 
  (Sphere, m, mt)

let intersect (r: ray) (tng: thing) : intersection list = 
  let obj, m, _ = tng in 
  let r = transform_ray r (inverse m) in 
  let org, dir = r in 
  match obj with 
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
  let shp, m, _ = tng in 
  match shp with 
  | Sphere -> 
    let obj_point = transform_point p (inverse m) in 
    let obj_normal = sub obj_point (make_point 0. 0. 0.) in 
    let wrld_normal = transform_point obj_normal (transpose (inverse m)) in 
    let a, b, c, _ = wrld_normal in let wrld_normal = make_vector a b c in 
    norm wrld_normal


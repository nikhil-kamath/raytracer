open Rays
open Features
open Matrix

type shape = Sphere
type thing = shape * matrix
type intersection = {t: float; o: thing}

let make_sphere (m: matrix option) : thing = 
  match m with 
  | None -> (Sphere, identity_of 4)
  | Some m -> (Sphere, m)

let intersect (r: ray) (tng: thing) : intersection list = 
  let obj, m = tng in 
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
  


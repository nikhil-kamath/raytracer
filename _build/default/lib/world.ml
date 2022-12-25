open Objects
open Lights
open Features
open Materials
open Transformations
open Rays
open Colors

type world = {
  objects: thing list;
  lights: light list;
}
type computation = {
  time: float;
  obj: thing;
  point: point;
  eyev: point;
  normalv: point;
  inside: bool;
}

let default_world () = 
  let l = {
    position = make_point (-10.) 10. (-10.);
    intensity = (1., 1., 1.)
  } in 
let s1 = make_sphere ~material:(make_material ~color:(0.8, 1., 0.6) ~diffuse:0.7 ~specular:0.2 () ) () in 
  let s2 = make_sphere ~transformation:(scale 0.5 0.5 0.5) () in 
  {objects = [s1; s2]; lights=[l]}

let intersect_world (w: world) (r: ray) : intersection list = 
  let is = List.concat (List.map (fun o -> intersect r o) w.objects) in 
  List.sort (fun x y -> compare x.t y.t) is


let prepare_computations (i: intersection) (r: ray) : computation = 
  let _, dir = r in 
  let pnt = position r i.t in 
  let ev = mult dir (-1.) in 
  let nv1 = normal_at i.o pnt in 
  let ins, nv = if dot nv1 ev < 0. then true, (mult nv1 (-1.)) else false, nv1 in 
  {
    time = i.t;
    obj = i.o;
    point = position r i.t;
    eyev = ev;
    normalv = nv;
    inside = ins;
  }

let shade_hit (w: world) (c: computation) : color = 
  let l = match w.lights with [] -> failwith "no light found!" | h::_ -> h in 
  lighting c.obj.material l c.point c.eyev c.normalv

let color_at (w: world) (r: ray) = 
  let is = intersect_world w r in 
  let h = hit is in 
  match h with 
  | None -> (0., 0., 0.) 
  | Some(i) -> shade_hit w (prepare_computations i r)

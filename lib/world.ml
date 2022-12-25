open Objects
open Lights
open Features
open Materials
open Transformations

type world = {
  objects: thing list;
  lights: light list;
}

let default_world () = 
  let l = {
    position = make_point (-10.) 10. (-10.);
    intensity = (1., 1., 1.)
  } in 
  let s1 = make_sphere ~material:(make_material ~color:(0.8, 1., 0.6) ~diffuse:0.7 ~specular:0.2 () ) () in 
  let s2 = make_sphere ~transformation:(scale 0.5 0.5 0.5) () in 
  {objects = [s1; s2]; lights=[l]}

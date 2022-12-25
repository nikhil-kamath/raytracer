open Objects
open Lights
open Features
open Materials

type world = {
  objects: thing list;
  lights: light list;
}

let default_world () = 
  let l = {
    position = make_point (-10.) 10. (-10.);
    intensity = (1., 1., 1.)
  } in 

  let s1 = make_sphere None (make_material ~color=(0.8, 1., 0.6) () )
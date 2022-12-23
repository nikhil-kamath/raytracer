open Display
open Features

type projectile = point * point

type environment = point * point

let tick (proj : projectile) (env : environment) : projectile = 
  let pos, vel = proj in 
  let gravity, wind = env in 
  (add pos vel), (add vel (add gravity wind));;

let rec simulate (proj : projectile) (env : environment) (disp : canvas) : canvas = 
  let pos, _ = proj in 
  let x, y, _, _ = pos in  
  if y < 0. then disp else
    let () = set_pixel disp (int_of_float y) (int_of_float x) (1., 0., 0.) in 
    simulate (tick proj env) env disp;; 
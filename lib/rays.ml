open Features
open Matrix

type ray = point * point

let make_ray (origin : point) (direction : point) : ray = 
  let _, _, _, p = origin in 
  let _, _, _, v = direction in 
  if abs_float (p-.1.) > epsilon then failwith "first parameter of make_ray should be a point" else
    if abs_float (v) > epsilon then failwith "second parameter of make_ray should be vector" else 
      origin, direction

let position (r: ray) (t: float) : point = 
  let origin, direction = r in 
  add origin (mult direction t)

let transform_ray (r: ray) (m: matrix) : ray = 
  let origin, direction = r in 
  (transform_point origin m, transform_point direction m)

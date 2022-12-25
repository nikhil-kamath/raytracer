open Matrix
open Rays
open Features
open World
open Display
open Util

type camera = {
  hsize: int;
  vsize: int;
  field_of_view: float;
  transformation: matrix;
  half_width: float;
  half_height: float;
  pixel_size: float;
  }
  
  let pixel_size (cam: camera) : float * float * float = 
    let half_view = Float.tan (cam.field_of_view /. 2.) in 
    let aspect = (float_of_int cam.hsize) /. (float_of_int cam.vsize) in 
    let half_width, half_height = 
      if aspect >= 1. then 
        half_view, half_view /. aspect
      else 
        half_view *. aspect, half_view
    in
    half_width, half_height, (half_width *. 2.) /. (float_of_int cam.hsize)

let make_camera (hs: int) (vs: int) (fov: float) ?(transformation=identity_of 4) () : camera = 
  let temp = {hsize=hs; vsize=vs; field_of_view=fov; transformation=transformation; half_width = 0.; half_height = 0.; pixel_size = 0.} in 
  let hw, hh, ps = pixel_size temp in 
  {hsize=hs; vsize=vs; field_of_view=fov; transformation=transformation; half_width = hw; half_height = hh; pixel_size = ps} 
  
let ray_for_pixel (cam: camera) (px: int) (py: int) : ray = 
  let xoffset = ((float_of_int px) +. 0.5) *. cam.pixel_size in 
  let yoffset = ((float_of_int py) +. 0.5) *. cam.pixel_size in 
  let world_x = cam.half_width -. xoffset in 
  let world_y = cam.half_height -. yoffset in 
  let pixel = transform_point (make_point world_x world_y (-1.)) (inverse cam.transformation) in
  let origin = transform_point (make_point 0. 0. 0.) (inverse cam.transformation) in 
  let direction = norm (sub pixel origin) in 
  make_ray origin direction

let render (cam: camera) (wrld: world) : canvas = 
  let disp = initialize_display cam.hsize cam.vsize in 
  List.iter (fun y ->
    List.iter (fun x ->
      let ray = ray_for_pixel cam x y in 
      let col = color_at wrld ray in 
      set_pixel disp x y col) 
    (range 0 cam.hsize))
  (range 0 cam.vsize); 
  disp
  


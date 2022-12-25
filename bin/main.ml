open Raytracer.Display
open Raytracer.Features
open Raytracer.Util
open Raytracer.Objects
open Raytracer.Rays
open Raytracer.Materials
open Raytracer.Lights
(* open Raytracer.Transformations
open Raytracer.Matrix *)

(* let _ = 
  let start = make_point 0. 1. 0. in 
  let velocity = mult (norm (make_vector 1. 1.8 0.)) 11.25 in 
  let proj : projectile = (start, velocity) in 
  let gravity = make_vector 0. (-0.1) 0. in 
  let wind = make_vector (-0.01) 0. 0. in 
  let env : environment = (gravity, wind) in 
  let disp = initialize_display 900 550 in 
  let finished = simulate proj env disp in 
  canvas_to_ppm finished "test1.ppm"  *)

(* let _ = 
  let thetas = linspace 0. (2. *. Float.pi) 12 in 
  let rotations = List.map (fun theta -> rotate_z theta) thetas in 
  let first = matrix_of_point (make_point 100. 0. 0.) in 
  let points = List.map (fun rts -> point_of_matrix (combine [translate 250. 250. 0.; rts] first)) rotations in
  let disp = initialize_display 500 500 in 
  List.iter (fun p -> 
    let x, y, _, _ = p in set_pixel disp (int_of_float y) (int_of_float x) (1., 0., 0.)) 
    points;
  canvas_to_ppm disp "test2.ppm" *)

let _ = 
  let ray_origin = make_point 0. 0. (-5.) in 
  let wall_z = 10. in 
  let wall_size = 7. in 
  let canvas_pixels = 500. in 
  let pixel_size = wall_size /. canvas_pixels in 
  let half = wall_size /. 2. in 
  let disp = initialize_display (int_of_float canvas_pixels) (int_of_float canvas_pixels) in 
  let mt = make_material ~color:(0.2, 0.2, 1.) () in
  let sphere = make_sphere ~material:mt () in 
  let lpos = make_point (-10.) 10. (-10.) in 
  let lcol = (1.,1.,1.) in 
  let l = make_light lpos lcol in 
  let ys = List.map float_of_int (range 0 (int_of_float canvas_pixels)) in
  let xs = List.map float_of_int (range 0 (int_of_float canvas_pixels)) in 
  let () = List.iter 
  (fun y -> 
    let world_y = half -. pixel_size *. y in 
    List.iter 
    (fun x ->
      let world_x = -.half +. pixel_size *. x in 
      let pstn = make_point world_x world_y wall_z in 
      let r = make_ray ray_origin (norm (sub pstn ray_origin)) in 
      let is = intersect r sphere in 
      match hit is with 
      | None -> ()
      | Some i -> 
        let p = position r i.t in 
        let nv = normal_at i.o p in 
        let _, ray_direction = r in 
        let eyev = mult ray_direction (-1.) in 
        let hit_color = lighting i.o.material l p eyev nv in
        set_pixel disp (int_of_float y) (int_of_float x) hit_color) 
      xs) 
    ys
  in canvas_to_ppm disp "test4.ppm"
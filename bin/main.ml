open Raytracer.Display
(* open Raytracer.Physics *)
open Raytracer.Features
open Raytracer.Util
open Raytracer.Objects
open Raytracer.Matrix
open Raytracer.Rays
open Raytracer.Transformations
open Raytracer.Colors

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
  let canvas_pixels = 100. in 
  let pixel_size = wall_size /. canvas_pixels in 
  let half = wall_size /. 2. in 
  let disp = initialize_display (int_of_float canvas_pixels) (int_of_float canvas_pixels) in 
  let red : color = (1., 0., 0.) in 
  let sphere = make_sphere (Some (combine [rotate_z (Float.pi /. 4.); scale 0.5 1. 1.])) in 
  let ys = List.map float_of_int (range 0 (int_of_float canvas_pixels)) in
  let xs = List.map float_of_int (range 0 (int_of_float canvas_pixels)) in 
  let () = List.iter 
  (fun y -> 
    let world_y = half -. pixel_size *. y in 
    List.iter 
    (fun x ->
      let world_x = -.half +. pixel_size *. x in 
      let position = make_point world_x world_y wall_z in 
      let r = make_ray ray_origin (sub position ray_origin) in 
      let is = intersect r sphere in 
      match hit is with 
      | None -> ()
      | Some _ -> set_pixel disp (int_of_float y) (int_of_float x) red) 
      xs) 
      ys
  in canvas_to_ppm disp "test3.ppm"
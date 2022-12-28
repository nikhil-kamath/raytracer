open Raytracer.Display
open Raytracer.Features
(* open Raytracer.Util *)
open Raytracer.Objects
(* open Raytracer.Rays *)
open Raytracer.Materials
open Raytracer.Lights
open Raytracer.Transformations
open Raytracer.Matrix
open Raytracer.Camera
open Raytracer.World
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
(* 
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
  in canvas_to_ppm disp "test4.ppm" *)

(* let _ = 
  let floor = make_sphere 
    ~transformation:(scale 10. 0.01 10.) 
    ~material:(make_material ~color:(rgb_to_color (70, 30, 34)) ~specular:0. ()) () in 
    let left_wall = make_sphere 
    ~transformation:(combine [
      translate 0. 0. 5.;
      rotate_y (-.Float.pi /. 4.);
      rotate_x (Float.pi /. 2.);
      scale 10. 0.01 10.]) 
    ~material:(make_material ~color:(rgb_to_color (244, 192, 149)) ~specular:0. ()) () in 
  let right_wall = make_sphere 
    ~transformation:(combine [
      translate 0. 0. 5.;
      rotate_y (Float.pi /. 4.);
      rotate_x (Float.pi /. 2.);
      scale 10. 0.01 10.]) 
    ~material:left_wall.material () in 
  let middle = make_sphere 
    ~transformation:(translate (-0.5) 1. 0.5)
    ~material:(make_material
      ~color:(rgb_to_color (29, 120, 116))
      ~diffuse:0.7
      ~specular:0.3 ()) () in 
  let right = make_sphere 
    ~transformation:(combine [
      translate (1.5) 0.5 (-0.5);
      scale 0.5 0.5 0.5])
    ~material:(make_material
      ~color:(rgb_to_color (254, 254, 227))
      ~diffuse:0.7
      ~specular:0.3 ()) () in 
  let left = make_sphere 
    ~transformation:(combine [
      translate (-1.5) 0.33 (-0.75);
      scale 0.33 0.33 0.33])
    ~material:(make_material
      ~color:(rgb_to_color (103, 146, 137))
      ~diffuse:0.7
      ~specular:0.3 ()) () in 
  let light = make_light (make_point (-10.) 10. (-10.)) (1., 1., 1.) in 
  let camera = make_camera 400 200 (Float.pi /. 3.) 
    ~transformation:(view 
      (make_point 0. 1.5 (-5.))
      (make_point 0. 1. 0.)
      (make_vector 0. 1. 0.)) () in 
  let world = {objects=[left_wall;floor;right_wall;left;middle;right]; lights=[light]} in 
  let output = render camera world in 
  canvas_to_ppm output "test5.ppm"

   *)

let _ = 
  let floor = make_plane 
    ~transformation:(rotate_z (Float.pi /. 12.)) () in 
  let middle = make_sphere 
    ~transformation:(translate (-0.5) 1. 0.5)
    ~material:(make_material
      ~color:(rgb_to_color (29, 120, 116))
      ~diffuse:0.7
      ~specular:0.3 ()) () in 
  let right = make_sphere 
    ~transformation:(combine [
      translate (1.5) 0.5 (-0.5);
      scale 0.5 0.5 0.5])
    ~material:(make_material
      ~color:(rgb_to_color (254, 254, 227))
      ~diffuse:0.7
      ~specular:0.3 ()) () in 
  let left = make_sphere 
    ~transformation:(combine [
      translate (-1.5) 0.33 (-0.75);
      scale 0.33 0.33 0.33])
    ~material:(make_material
      ~color:(rgb_to_color (103, 146, 137))
      ~diffuse:0.7
      ~specular:0.3 ()) () in 
  let light = make_light (make_point (-10.) 2.5 (-10.)) (1., 1., 1.) in 
  let camera = make_camera 400 200 (Float.pi /. 3.) 
    ~transformation:(view 
      (make_point 0. 1.5 (-5.))
      (make_point 0. 1. 0.)
      (make_vector 0. 1. 0.)) () in 
  let world = {objects=[floor;left;middle;right]; lights=[light]} in 
  let output = render camera world in 
  canvas_to_ppm output "test6.ppm"


open OUnit2
open Raytracer.Features
open Raytracer.Colors

let epsilon = 0.00000001;;
let equal_c (a: color) (b: color) : bool = 
  let q, w, e = a in 
  let a, s, d = b in 
  abs_float (q-.a) < epsilon &&
  abs_float (w-.s) < epsilon &&
  abs_float (e-.d) < epsilon 

let print_color (a: color) : unit = 
  let a, b, c = a in
  print_string "("; 
  print_float a;
  print_string ", ";
  print_float b;
  print_string ", ";
  print_float c;
  print_string ")";;

print_color (0., 0., 0.);;

let test_addition _ = 
  let a = make_point 3. (-2.) 5. in 
  let b = make_vector (-2.) 3. 1. in 
  assert_equal (add a b) (make_point 1. 1. 6.);;

let test_subtraction _ = 
  let a = make_point 3. 2. 1. in 
  let b = make_point 5. 6. 7. in 
  assert_equal (sub a b) (make_vector (-2.) (-4.) (-6.));
  let a = make_point 3. 2. 1. in 
  let b = make_vector 5. 6. 7. in 
  assert_equal (sub a b) (make_point (-2.) (-4.) (-6.));
  let a = make_vector 3. 2. 1. in 
  let b = make_vector 5. 6. 7. in 
  assert_equal (sub a b) (make_vector (-2.) (-4.) (-6.))

let test_multiplication _ = 
  let a = make_point 1. 2. 3. in 
  assert_equal (mult a 2.) (make_point 2. 4. 6.)

let test_division _ = 
  let a = make_vector 2. 4. 6. in 
  assert_equal (div a 2.) (make_vector 1. 2. 3.)

let test_magnitude _ = 
  let a = make_vector (-1.) (-2.) (-3.) in 
  assert_equal (mag a) (sqrt 14.)

let test_normalize _ = 
  let a = make_vector 1. 2. 3. in 
  assert_equal (norm a) (make_vector (1. /. (sqrt 14.)) (2. /. (sqrt 14.)) (3. /. (sqrt 14.)))

let test_dot _ = 
  let a = make_vector 1. 2. 3. in 
  let b = make_vector 4. 5. 6. in 
  assert_equal (dot a b) (32.)

let test_cross _ = 
  let a = make_vector 1. 2. 3. in 
  let b = make_vector 4. 5. 6. in 
  assert_equal (cross a b) (make_vector (-3.) 6. (-3.))

let test_colors _ =
  let a : color = (1., 0.2, 0.4) in 
  let b : color = (0.9, 1., 0.1) in 
  let s = 2. in  
  assert_equal true (equal_c (mult_c a b) (0.9, 0.2, 0.04));
  assert_equal true (equal_c (scale_c a s) (2., 0.4, 0.8));
  assert_equal true (equal_c (sub_c a b) (0.1, -0.8, 0.3))


let suite = 
  "tuple testing"
  >::: ["tuple_add" >:: test_addition;
        "tuple_subtract" >:: test_subtraction;
        "tuple_multiply" >:: test_multiplication;
        "tuple_divide" >:: test_division;
        "vector_magnitude" >:: test_magnitude;
        "vector_normalize" >:: test_normalize;
        "vector_dot" >:: test_dot;
        "vector_cross" >:: test_cross;
        "color_ops" >:: test_colors;
        ]

let _ = run_test_tt_main suite;;



open Features

type color = float * float * float

let color_to_vector (a: color) : point = 
  let r, g, b = a in make_vector r g b

let vector_to_color (a: point) : color =
  let r, g, b, _ = a in (r, g, b)

let add_c (a: color) (b: color) : color = 
  vector_to_color (add (color_to_vector a) (color_to_vector b)) 

let sub_c (a: color) (b: color) : color = 
  vector_to_color (sub (color_to_vector a) (color_to_vector b))

let scale_c (a: color) (s: float) : color = 
  vector_to_color (mult (color_to_vector a) s) 

let mult_c (first: color) (second: color) : color = 
  let r, g, b = first in 
  let r2, g2, b2 = second in 
  (r*.r2, g*.g2, b*.b2)

let rgb_to_color rgb : color = 
  let r, g, b = rgb in 
  (float_of_int r) /. 255., (float_of_int g) /. 255., (float_of_int b) /. 255.
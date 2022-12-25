open Colors

type canvas = color array array;;

let initialize_display w h : canvas= 
  Array.make_matrix h w (0., 0., 0. : color)
  
let get_pixel (c: canvas) (row : int) (col : int) : color = 
  c.(row).(col)

let height (c: canvas) = 
  Array.length c

let width (c: canvas) = 
  Array.length c.(0)

let set_pixel (c: canvas) (x: int) (y: int) (new_color: color) : unit = 
  if x >= width c || y >= height c then 
    failwith ("cannot add point (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")") 
  else
    c.(y).(x) <- new_color

let clamp n a b : int = 
  if n < a then a else
    if n > b then b else
      n
      
let color_to_pixel (c: color) = 
  let r, g, b = c in 
  let r = int_of_float (r*. 255.) in 
  let g = int_of_float (g*. 255.) in 
  let b = int_of_float (b*. 255.) in 
  let r = clamp r 0 255 in 
  let g = clamp g 0 255 in 
  let b = clamp b 0 255 in 
  (r, g, b)

let print_row (row : color array) (file : out_channel) : unit = 
  Array.iter (fun col -> 
    let r, g, b = color_to_pixel col in 
    Printf.fprintf file "%d %d %d " r g b)
    row

let print_rows (c : canvas) (file : out_channel) : unit = 
  Array.iter (fun row -> print_row row file;
  Printf.fprintf file "\n") c

let canvas_to_ppm (c: canvas) (filename: string) : unit = 
  let oc = open_out filename in 
  Printf.fprintf oc "P3\n";
  Printf.fprintf oc "%d %d\n" (width c) (height c);
  Printf.fprintf oc "255\n";
  print_rows c oc


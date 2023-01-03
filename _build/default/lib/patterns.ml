open Colors
open Matrix
open Features

type design = 
  Stripe of pattern * pattern |
  Gradient of pattern * pattern | 
  Ring of pattern * pattern |
  Checker of pattern * pattern | 
  Solid of color 

and pattern = {
  design:design;
  transformation:matrix;
}


let stripe_pattern (a: color) (b: color) ?(transformation=identity_of 4) (): pattern= 
  {design=Stripe({design=Solid(a); transformation = identity_of 4}, {design=Solid(b); transformation = identity_of 4}); transformation=transformation}

let gradient_pattern (a: color) (b: color) ?(transformation=identity_of 4) (): pattern= 
  {design=Gradient({design=Solid(a); transformation = identity_of 4}, {design=Solid(b); transformation = identity_of 4}); transformation=transformation}

let ring_pattern (a: color) (b: color) ?(transformation=identity_of 4) (): pattern= 
  {design=Ring({design=Solid(a); transformation = identity_of 4}, {design=Solid(b); transformation = identity_of 4}); transformation=transformation}

let checker_pattern (a: color) (b: color) ?(transformation=identity_of 4) (): pattern= 
  {design=Checker({design=Solid(a); transformation = identity_of 4}, {design=Solid(b); transformation = identity_of 4}); transformation=transformation}



let rec color_on_design (pt: pattern) (pattern_point: point) : color = 
  let pattern_point = transform_point pattern_point (inverse pt.transformation) in 
  match pt.design with 
  | Stripe(ca, cb) ->
    let ca = color_on_design ca pattern_point in 
    let cb = color_on_design cb pattern_point in 
    let x, _, _, _ = pattern_point in 
    if mod_float (floor x) 2. = 0. then ca else cb
  | Gradient(ca, cb) ->
    let ca = color_on_design ca pattern_point in 
    let cb = color_on_design cb pattern_point in 
    let x, _, _, _ = pattern_point in 
    add_c ca (scale_c (sub_c cb ca) (x -. floor x))
  | Ring(ca, cb) ->
    let ca = color_on_design ca pattern_point in 
    let cb = color_on_design cb pattern_point in 
    let x, _, z, _ = pattern_point in 
    if int_of_float (sqrt ((Float.pow x 2.) +. (Float.pow z 2.))) mod 2 = 0 then ca else cb
  | Checker(ca, cb) ->
    let ca = color_on_design ca pattern_point in 
    let cb = color_on_design cb pattern_point in 
    let x, y, z, _ = pattern_point in 
    if mod_float (List.fold_left (+.) 0. (List.map floor [x; y; z])) 2. = 0. then ca else cb 
  | Solid(c) -> c

    
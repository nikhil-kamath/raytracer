open Colors
open Features
open Materials
open Objects

type light = {position: point; intensity: color}

let make_light (pos: point) (i: color) : light = 
  {position=pos; intensity=i}

let lighting (mt: material) (obj: thing) (l: light) (p: point) (ev: point) (nv: point) (in_shadow: bool): color =
  let surface_color = pattern_color mt.pattern obj p in 
  let ecolor = mult_c surface_color l.intensity in 
  let lv = norm (sub l.position p) in 
  let amb = scale_c ecolor mt.ambient in 
  let ldotn = dot lv nv in 
  let (dif: color), (spec: color) = 
    if ldotn < 0. then (0., 0., 0.), (0., 0., 0.) else 
      let rv = reflect (mult lv (-1.)) nv in 
      let rdote = dot rv ev in 
      if rdote <= 0. then 
        scale_c ecolor (ldotn *. mt.diffuse), (0., 0., 0.)
      else 
        let f = Float.pow rdote mt.shininess in 
        scale_c ecolor (ldotn *. mt.diffuse), scale_c l.intensity (mt.specular *. f)
  in 
  if in_shadow then amb else add_c amb (add_c dif spec)

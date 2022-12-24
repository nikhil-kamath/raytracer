open Colors
open Features
open Materials

type light = {position: point; intensity: color}

let make_light (pos: point) (i: color) : light = 
  {position=pos; intensity=i}

let lighting (mt: material) (l: light) (p: point) (ev: point) (nv: point) : color =
  let ecolor = mult_c mt.clr l.intensity in 
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
  add_c amb (add_c dif spec)

open Colors

type material = {
  clr: color;
  ambient: float;
  diffuse: float;
  specular: float;
  shininess: float;
}

let make_material (c: color) a d s sh = 
  {clr = c;
  ambient = a;
  diffuse = d;
  specular = s;
  shininess = sh}

let default_material = 
  make_material (1., 1., 1.) 0.1 0.9 0.9 200.
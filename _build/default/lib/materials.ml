open Colors

type material = {
  clr: color;
  ambient: float;
  diffuse: float;
  specular: float;
  shininess: float;
}

let make_material ?(color:color=(1., 1., 1.)) ?(ambient=0.1) ?(diffuse=0.9) ?(specular=0.9) ?(shininess=200.) (): material = 
  {
    clr=color;
    ambient=ambient;
    diffuse=diffuse;
    specular=specular;
    shininess=shininess
  }

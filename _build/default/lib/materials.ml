open Colors
open Patterns
open Matrix

type material = {
  clr: color;
  ambient: float;
  diffuse: float;
  specular: float;
  shininess: float;
  pattern: pattern;
}

let make_material ?(color:color=(1., 1., 1.)) ?(ambient=0.1) ?(diffuse=0.9) ?(specular=0.9) ?(shininess=200.) ?(pattern={design=Solid(0.,0.,0.); transformation=identity_of 4}) (): material = 
  let pattern = match pattern.design with 
  | Solid(_) -> {design = Solid(color); transformation = identity_of 4}
  | _ -> pattern in 
  {
    clr=color;
    ambient=ambient;
    diffuse=diffuse;
    specular=specular;
    shininess=shininess;
    pattern=pattern;
  }

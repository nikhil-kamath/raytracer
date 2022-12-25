open Matrix
open Features


let translate x y z = 
  matrix_of [
    [1.; 0.; 0.; x];
    [0.; 1.; 0.; y];
    [0.; 0.; 1.; z];
    [0.; 0.; 0.; 1.]]

let scale x y z = 
  matrix_of [
    [x; 0.; 0.; 0.];
    [0.; y; 0.; 0.];
    [0.; 0.; z; 0.];
    [0.; 0.; 0.; 1.]]

let rotate_x theta = 
    matrix_of [
    [1.; 0.; 0.; 0.];
    [0.; cos theta; -. (sin theta); 0.];
    [0.; sin theta; cos theta; 0.];
    [0.; 0.; 0.; 1.]]

let rotate_y theta = 
    matrix_of [
    [cos theta; 0.; sin theta; 0.];
    [0.; 1.; 0.; 0.];
    [-. (sin theta); 0.; cos theta; 0.];
    [0.; 0.; 0.; 1.]]

let rotate_z theta = 
  matrix_of [
    [cos theta; -. (sin theta); 0.; 0.];
    [sin theta; cos theta; 0.; 0.];
    [0.; 0.; 1.; 0.];
    [0.; 0.; 0.; 1.]]
  
let shear xy xz yx yz zx zy = 
    matrix_of [
    [1.; xy; xz; 0.];
    [yx; 1.; yz; 0.];
    [zx; zy; 1.; 0.];
    [0.; 0.; 0.; 1.]]

let view (f: point) (t: point) (up: point) : matrix = 
  let forward = norm (sub t f) in 
  let left = cross forward (norm up) in 
  let true_up = cross left forward in 
  let lx, ly, lz, _ = left in 
  let tux, tuy, tuz, _ = true_up in 
  let fx, fy, fz, _ = forward in 
  let fromx, fromy, fromz, _ = f in 
  mult_m (matrix_of [
    [lx; ly; lz; 0.];
    [tux; tuy; tuz; 0.];
    [-.fx; -.fy; -.fz; 0.];
    [0.; 0.; 0.; 1.]
  ])
  (translate (-.fromx) (-.fromy) (-.fromz))

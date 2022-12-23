open Matrix


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
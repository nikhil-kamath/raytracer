let range a b = 
  List.init (b-a) (fun x -> x+a)

let linspace a b steps = 
  let width = b -. a in 
  let l = range 0 steps in 
  List.map (fun x -> (float_of_int x) *. width /. float_of_int steps +. a) l
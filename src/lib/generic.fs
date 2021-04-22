module generic

open lib.matrix
open lib.vector

let v = Vector([|1;2|])

//let inline dot (Va:Vector<'T>) (Vb:Vector<'T>) = Array.fold2 (fun x y z -> x + (y * z)) 0 Va.data Vb.data
let inline dot (Va:Vector<'T>) (Vb:Vector<'T>) = Array.map2 (fun x y -> x * y) Va.data Vb.data |> Array.sum



module linearalgebra

open lib.matrix

let checksquare (M:Matrix<'T>) = 
    if M.length1 = M.length2 then M.length1
    else failwith "Dimension mismatch: matrix is not square"




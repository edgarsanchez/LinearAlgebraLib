module adjtrans

open lib.matrix

type Transpose<'T>(_parent: Matrix<'T>) = 
    member this.parent = _parent
    
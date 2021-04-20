namespace lib

module vector = 
    type Vector<'T>(_data: array<'T>) = 
        member this.data = _data
        member this.Item
            with get (index:int) = this.data.[index]
            and set (index:int) (value:'T) = this.data.[index] <- value

        //vec + vec
        static member inline (+) (Va:Vector<'a>, Vb:Vector<'a> ) = Vector(Array.map2 (+) Va.data Vb.data)

        //vec - vec
        static member inline (-) (Va:Vector<'a>, Vb:Vector<'a>) = Vector(Array.map2 (-) Va.data Vb.data)
    
        //-vec
        static member inline (~-) (V:Vector<'a>) = Vector(Array.map (fun x -> -x) V.data)

        //vec * es
        static member inline (*) (V:Vector<'a>, es:'a) = Vector(Array.map (fun x -> x * es) V.data)

        //es * vec
        static member inline (*) (es:'a, V:Vector<'a>) = Vector(Array.map (fun x -> x * es) V.data)   

        //vec / es
         static member inline (/) (V:Vector<'a>, es:'a) = Vector(Array.map (fun x -> x / es) V.data)

module matrix = 
    open vector
    type Matrix<'T>(_data: 'T[,]) = 
        member this.data = _data
        member this.length1 = Array2D.length1 this.data
        member this.length2 = Array2D.length2 this.data

        //mat + mat
        static member inline (+) (Ma:Matrix<'a>, Mb:Matrix<'a>) = Matrix(Array2D.init Ma.length1 Ma.length2 (fun i j -> Ma.data.[i,j] + Mb.data.[i,j]))

        //mat - mat
        static member inline (-) (Ma:Matrix<'a>, Mb:Matrix<'a>) = Matrix(Array2D.init Ma.length1 Ma.length2 (fun i j -> Ma.data.[i,j] - Mb.data.[i,j]))

        //-mat
        static member (~-) (M:Matrix<'a>) = Matrix(Array2D.map (fun x -> -x) M.data)

        // mat * mat
        static member inline (*) (Ma:Matrix<'a>, Mb:Matrix<'a>) = 
            Array2D.init Ma.length1 Mb.length2 (fun i j -> Array.sum (Array.map2 (*) Ma.data.[i,*] Mb.data.[*,j]))
    
        //mat * vec
        static member inline (*) (M:Matrix<'a>, V:Vector<'a>) = 
            Array2D.init M.length1 V.data.Length (fun i j -> Array.sum (Array.map2 (*) M.data.[i,*] V.data))
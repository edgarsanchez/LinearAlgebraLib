module lu

open lib.matrix
open lib.vector

let m = Matrix<float>([| [|2.;1.|] ; [|8.;7.|] |])

let x = [| [|5.;4.;-7.|] ; [|5.;-8.;1.|] ; [|-1.;2.;-3.|] |]

type LU<'T>(_factors:Matrix<'T>, _ipiv:Vector<int>, _info:int) = 
    member this.factors = _factors
    member this.ipiv = _ipiv
    member this.info = _info

let lu (A:Matrix<'T>) =
    let Pivot = true
    let m = A.length1
    let n = A.length2
    let mutable info = 0
    let minmn = min m n
    let ipiv = Vector(Array.zeroCreate minmn)
    let mutable tmp = 0.0
    let mutable kp = 1
    let mutable absi = 0.0
    let mutable amax = 0.0
    for k in 1..minmn do
        kp <- k
        if Pivot && k < m then
            amax <- abs A.[k,k]
            for i in k+1..m do
                absi <- abs A.[i,k]
                if absi > amax then
                    kp <- i
                    amax <- absi
        ipiv.[k] <- kp
        if not(A.[kp,k] = 0.0) then
            if not(k = kp) then
                for i in 1..n do
                    tmp <- A.[k,i]
                    A.[k,i] <- A.[kp,i]
                    A.[kp,i] <- tmp
            let Akkinv = 1.0 / A.[k,k]
            for i in k+1..m do
                A.[i,k] <- A.[i,k] * Akkinv
        elif info = 0 then
            info <- k
        
        for j in k+1..n do
            for i in k+1..m do
                A.[i,j] <- A.[i,j] - (A.[i,k]*A.[k,j])
        
    LU<'T>(A, ipiv, info)

let b = lu(m)


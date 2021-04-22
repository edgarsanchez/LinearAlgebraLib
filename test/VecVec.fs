module VecVec

open System
open Xunit
open lib.vector

[<Fact>]
let ``vec + vec (Int)`` () =
    let v1 = Vector([|1;2;3|])
    let v2 = Vector([|10;20;30|])
    let v3 = Vector([|0;0;0|])
    
    //v1 + v1
    let mutable ans = v1 + v1
    Assert.True(ans.data = [|2;4;6|])

    //v1 + v2
    let ans = v1 + v2
    Assert.True(ans.data = [|11;22;33|])

    //v1 + v3
    let ans = v1 + v3
    Assert.True(ans.data = [|1;2;3|])

    //v3 + v3
    let ans = v3 + v3
    Assert.True(ans.data = [|0;0;0|])

[<Fact>]
let ``vec + vec (Float)`` () = 
    let v1 = Vector([|1.1;2.2;3.3|])
    let v2 = Vector([|10.0;20.0;30.0|])
    let v3 = Vector([|0.0;0.0;0.0|])

    //v1 + v1
    let mutable ans = v1 + v1
    Assert.True(ans.data = [|2.2;4.4;6.6|])

    //v1 + v2
    let ans = v1 + v2
    Assert.True(ans.data = [|11.1;22.2;33.3|])

    //v1 + v3
    let ans = v1 + v3
    Assert.True(ans.data = [|1.1;2.2;3.3|])

    //v3 + v3
    let ans = v3 + v3
    Assert.True(ans.data = [|0.0;0.0;0.0|])

[<Fact>]
let ``vec - vec (Int)`` () =
    let v1 = Vector([|1;2;3|])
    let v2 = Vector([|10;20;30|])
    let v3 = Vector([|0;0;0|])
    
    //v1 - v1
    let mutable ans = v1 - v1
    Assert.True(ans.data = [|0;0;0|])

    //v1 - v2
    let ans = v1 - v2
    Assert.True(ans.data = [|9;18;27|])

    //v1 - v3
    let ans = v1 - v3
    Assert.True(ans.data = [|1;2;3|])

    //v3 - v3
    let ans = v3 - v3
    Assert.True(ans.data = [|0;0;0|])

[<Fact>]
let ``vec - vec (Float)`` () = 
    let v1 = Vector([|1.1;2.2;3.3|])
    let v2 = Vector([|10.0;20.0;30.0|])
    let v3 = Vector([|0.0;0.0;0.0|])

    //v1 - v1
    let mutable ans = v1 - v1
    Assert.True(ans.data = [|0.0;0.0;0.0|])

    //v1 - v2
    let ans = v1 - v2
    Assert.True(ans.data = [|-8.9;-17.8;-26.7|])

    //v1 - v3
    let ans = v1 - v3
    Assert.True(ans.data = [|1.1;2.2;3.3|])

    //v3 - v3
    let ans = v3 - v3
    Assert.True(ans.data = [|0.0;0.0;0.0|])
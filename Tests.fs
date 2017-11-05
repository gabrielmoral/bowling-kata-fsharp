module Tests

open Xunit
open Game

[<Fact>]
let ``Simple rolls`` () =
    Assert.Equal(1, score "1")
    Assert.Equal(3, score "3")
    Assert.Equal(9, score "333")

[<Fact>]
let ``Missed rolls`` () =
    Assert.Equal(1, score("1-"))
    Assert.Equal(6, score("1-1-1-1-1-1"))

//[<Fact>]
// let ``Spares`` () =
//     Assert.Equal(10, score("1/"))
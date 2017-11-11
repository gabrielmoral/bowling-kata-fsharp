module Tests

open Xunit
open Game

[<Fact>]
let ``Simple rolls`` () =
    Assert.Equal(2, score "11")
    Assert.Equal(6, score "33")

[<Fact>]
let ``Missed rolls`` () =
    Assert.Equal(1, score("1-"))
    Assert.Equal(6, score("1-1-1-1-1-1-"))

[<Fact>]
let ``Spares`` () =
    Assert.Equal(10, score("1/"))       
    Assert.Equal(25, score("1/55"))
    Assert.Equal(109, score("1/1/1/1/1/1/1/1/1/1/"))

[<Fact>]
let ``Mix roll types``() =
    Assert.Equal(39, score("11--3-1/--1/1/11"))
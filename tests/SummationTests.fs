module SummationTests

open System
open Xunit
open Lib

[<Fact>]
let ``Multiplication list folder`` () =
    let input = [1;2;3;4;5;]
    let actual = Summation.multList input
    Assert.Equal(120, actual)

[<Fact>]
let ``Find sum tests`` () = 
    let input = [1721;979;366;299;675;1456]
    let rec fn = function
    | [] -> Error "Processs could not find sum"
    | (x::xs) -> 
        match (Summation.findSum x 2020 xs) with
        | Ok x -> Ok x
        | Error _ -> fn xs
    let actual = fn input

    match actual with
    | Error _ -> Assert.True(false)
    | Ok x -> 
        Assert.Equal((List.length x) , 2)
        Assert.Contains(1721, x)
        Assert.Contains(299, x)

[<Fact>]
let ``List summation with doubles`` () =
    let input = [1721;979;366;299;675;1456]
    let actual = Summation.processInputDoubles 2020 input
    match actual with 
    | Ok x -> Assert.Equal(514579, x)
    | Error _ -> Assert.True(false)
    
[<Fact>]
let ``List summation with triples`` () =
    let input = [1721;979;366;299;675;1456]
    let actual = Summation.processInputTriples 2020 input
    match actual with 
    | Ok x -> Assert.Equal(241861950, x)
    | Error _ -> Assert.True(false)
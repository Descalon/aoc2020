module PolicyTests

open System
open Xunit
open Lib

[<Fact>]
let ``Parse should succeed`` () =
    let input = "1-3 a: aaab"
    let result = PolicyParser.parse input
    match result with
    | Ok _ -> Assert.True true
    | Error _ -> Assert.True false

[<Fact>]
let ``Parse should fail`` () =
    let input = "1-3 a: bbbbb"
    let result = PolicyParser.parse input
    match result with
    | Ok _ -> Assert.True false
    | Error _ -> Assert.True true

[<Fact>]
let ``Example test`` () =
    let input = ["1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc";]
    let result = 
        input 
        |> List.map PolicyParser.parse 
        |> List.filter (fun x -> 
            match x with
            | Ok _ -> true
            | Error _ -> false)
    
    Assert.Equal(2, List.length result)

[<Fact>]
let ``Example test part 2`` () =
    let input = ["1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc";]
    let result = 
        input 
        |> List.map PolicyParser.parse'
        |> List.filter (fun x -> 
            match x with
            | Ok _ -> true
            | Error _ -> false)
    
    Assert.Equal(1, List.length result)
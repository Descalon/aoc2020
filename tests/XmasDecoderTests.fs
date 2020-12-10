module XmasDecoderTests

open Xunit
open Lib.XmasDecoder
open Result

let sampleData = [35L;20L;15L;25L;47L;40L;62L;55L;65L;95L;102L;117L;150L;182L;127L;219L;299L;277L;309L;576L;]

let assertInt (a:int64) (b:int64) = Assert.Equal(a,b)

[<Fact>]
let ``Sample data test`` () =
    sampleData
    |> findFirstNonContained (splitPreamble 5)
    |> Result.bind (assertInt 127L >> Ok) |> ignore

[<Fact>]
let ``Test Exploit with sample`` () =
    let result = exploit 127L sampleData
    
    match result with
    | Error x -> Assert.True(false,x)
    | Ok x -> assertInt x 62L
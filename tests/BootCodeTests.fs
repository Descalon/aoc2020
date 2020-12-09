module BootCodeTests

open Xunit
open Lib.BootCode
open FParsec

let sampleInput = [
    "nop +0"
    "acc +1"
    "jmp +4"
    "acc +3"
    "jmp -3"
    "acc -99"
    "acc +1"
    "jmp -4"
    "acc +6"
]

let assertInt (a:int) (b:int) = Assert.Equal(a,b)

[<Theory>]
[<InlineData("acc +1", 1)>]
[<InlineData("acc -1", -1)>]
let ``Parsing should preserve sign`` (input, expected) =
    match (parseSingleOp input) with
    | Acc x -> assertInt expected x
    | _ -> Assert.True false

[<Fact>]
let ``Testing sample program`` () =
    sampleInput
    |> parseBootCode
    |> toProgram
    |> checkRepeatingProgram
    |> assertInt 5

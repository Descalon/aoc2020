// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Lib
open System.IO

let rBind (fn: 'a -> Result<'b, string>) x = 
    match x with
    | Ok a -> fn a
    | Error e ->
        printfn "Recieved error %s" e 
        Error e

let day1 () =
    Data.day1
    |> Summation.processInputDoubles 2020
    |> rBind (fun x -> printfn "Day1 solution = %i" x; Ok ())
    |> ignore

let day1' () =
    Data.day1
    |> Summation.processInputTriples 2020
    |> rBind (fun x -> printfn "Day1 triple solution = %i" x; Ok () )
    |> ignore

let day2 () =
    let result = Data.day2
                    |> List.map PolicyParser.parse 
                    |> List.filter (fun x -> 
                        match x with
                        | Ok _ -> true
                        | Error _ -> false)
    printfn "No of passwords = %i" (List.length result)

let day2' () =
    let result = Data.day2
                    |> List.map PolicyParser.parse' 
                    |> List.filter (fun x -> 
                        match x with
                        | Ok _ -> true
                        | Error _ -> false)
    printfn "No of passwords for part 2 = %i" (List.length result)

let day3 () =
    Data.day3
    |> Toboggan.processToboggan (3,1)
    |> printfn "Result of day 3 = %i"

let day3' () =
    Data.day3
    |> Toboggan.processToboggan' [(1,1); (3,1); (5,1); (7,1); (1,2)]
    |> printfn "Result of day 3 part 2 = %i"

let day4 () =
    Data.day4
    |> List.map PassportProcessing.checkPassport
    |> List.sumBy (fun x -> if x then 1 else 0)
    |> printfn "Result of day 4 = %i"

let selectId (s: BPScanner.SeatInfo) = s.Id

let day5() =
    Data.day5
    |> Seq.map BPScanner.calculateSeat
    |> Seq.maxBy (fun (x: BPScanner.SeatInfo) -> x.Id)
    |> selectId
    |> printfn "Result of day 5 = %i" 

let flip fn a b = fn b a

let day5'() =
    (Seq.map (BPScanner.calculateSeat >> selectId) Data.day5)
    |> (flip Seq.except) [0 .. 1023]
    |> Seq.toList
    |> List.iter (printfn "%i")

let day6() =
    Data.day6
    |> List.map (Questionaire.union)
    |> List.sumBy (Set.count)
    |> printfn "Result of day 6 = %i"

let day6'() =
    Data.day6
    |> List.map (Questionaire.intersection)
    |> List.sumBy (Set.count)
    |> printfn "Result of day 6 part 2 = %i"

let day8() =
    ["nop +0";"acc +1";"jmp +4";"acc +3";"jmp -3";"acc -99";"acc +1";"jmp -4";"acc +6";]
    |> BootCode.parseBootCode
    |> BootCode.toProgram
    |> BootCode.checkRepeatingProgram
    |> printfn "Result of day 8 = %i"

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        day1()
    else 
        match argv.[0].ToLowerInvariant() with
        | "day1" -> 
            day1 ()
            day1' ()
        | "day2" ->
            day2()
            day2'()
        | "day3" ->
            day3()
            day3'() 
        | "day4" ->
            day4()
        | "day5" ->
            day5()
            day5'()
        | "day6" ->
            day6()
            day6'()
        | "day8" ->
            day8()
        | _ -> raise (ArgumentException("No day recognised"))
    0 
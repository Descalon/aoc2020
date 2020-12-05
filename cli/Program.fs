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


let dataday4 : string list list = 
    let s = seq {
        use sr = new StreamReader("datafileDay4.txt")
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let rec split all passports = function
    | [] -> all @ [passports]
    | (x::xs) ->
        if System.String.IsNullOrWhiteSpace(x) then
            let all' = all @ [passports]
            split all' [] xs
        else 
            let passports' = passports @ [x.Trim()]
            split all passports' xs
    
    let n = split [] [] (s |> Seq.toList)
    printfn "%O" n
    n

let day4 () =
    dataday4
    |> List.map PassportProcessing.checkPassport
    |> List.sumBy (fun x -> if x then 1 else 0)
    |> printfn "Result of day 4 = %i"

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
        | _ -> raise (ArgumentException("No day recognised"))
    0 
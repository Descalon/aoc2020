// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Lib

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

// Define a function to construct a message to print
[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        day1()
    else 
        match argv.[0].ToLowerInvariant() with
        | "day1" -> 
            day1 ()
            day1' ()
        | _ -> raise (ArgumentException("No day recognised"))
    0 
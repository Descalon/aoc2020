namespace Lib

module Summation = 
    type ResultList<'a> = list<'a * 'a * 'a>

    let multList = List.fold(*) 1

    let processInput take (input: int list) matchWith =
        let rec fn c = 
            let i' = 
                input
                |> List.skip c
                |> List.take take 
            let sum = i' |> List.sum
            match sum with
            | x when x = matchWith -> List.foldBack (*) i' 1
            | _ -> fn (c + 1)
        
        fn 0
    
    let rec findSum left sum = function
    | [] -> Error "No sum found"
    | (x::xs) ->
        match (left + x) with
        | y when y = sum -> Ok [left; x]
        | _ -> findSum left sum xs

    let processInputDoubles matchWith (input: int list)= 
        let rec fn = function
        | [] -> Error "Processs could not find sum"
        | (x::xs) -> 
            match (findSum x matchWith xs) with
            | Ok x -> (multList >> Ok) x
            | Error _ -> fn xs
        fn input

    
    let processInputTriples matchWith (input: int list) =
        let rec fn = function
        | [] -> Error "Process could not find sum"
        | (x::xs) ->
            match(processInputDoubles (matchWith - x) xs) with
            | Ok y -> Ok (x * y)
            | Error _ -> fn xs
        fn input
namespace Lib

module XmasDecoder =

    let (>>=) a f= Result.bind f a
    let splitPreamble count input = List.take count input, List.skip count input
    let splitPreamble25 input = splitPreamble 25 input

    let calculateValidSums (input: int64 list) = 
        input
        |> List.allPairs input
        |> List.filter (fun (x,y) -> x <> y) 
        |> List.map (fun (x,y) -> x + y)

    let validate i = List.contains i

    let findFirstNonContained splitFn input =
        let rec fn input preamble = 
            let sums = calculateValidSums preamble
            match input with
            | [] -> Error "All values are contained"
            | (x::xs) ->
                match (validate x sums) with
                | false -> Ok x
                | true ->
                    let p' = preamble.Tail @ [x]
                    fn xs p'
        
        let (p, i) = splitFn input
        fn i p

    let exploit (target:int64) input = 
        let rec fn c = function
        | [] -> Error "no exploits found"
        | (x::xs) ->
            let c' = c |> List.map (List.append [x]) |> List.append [[x]]
            let s = c' |> List.filter (fun l -> (List.sum l) = target)
            match s with 
            | [] -> fn c' xs
            | [x] -> Ok x
            | _ -> Error "multiple exploits found"

        fn [] input
        >>= (fun x -> Ok (List.min x, List.max x))
        >>= (fun (x,y) -> Ok (x+y))
        

    

    let processData = findFirstNonContained splitPreamble25
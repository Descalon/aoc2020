namespace Lib

open FParsec

module BootCode =
    type Instruction = 
    | Acc   of int
    | Jump  of int
    | Nop   of int

    let toString = function
    | Acc x     -> sprintf "acc %i" x
    | Jump x    -> sprintf "jmp %i" x
    | Nop x     -> sprintf "nop %i" x

    type InstructionWithRunCount = Instruction * int
    type Program = Map<int,Instruction>

    let toProgram (is: Instruction list) = Map <| seq {
        for i in 0 .. (List.length is)-1 do yield (i, List.item i is)
    }

    let parseOp x cons = pstring x >>. (spaces >>. pint32) |>> cons

    let parseOps = choice [
        parseOp "acc" Acc
        parseOp "jmp" Jump
        parseOp "nop" Nop
    ]

    let parseSingleOp input =
        match (run parseOps input) with
        | Failure _ -> raise (System.ArgumentException("Why"))
        | Success (x,_,_) -> x
    
    let parseBootCode (input: string list) =
        input
        |> List.choose (fun x ->
            match (run parseOps x) with
            | Failure _ -> None
            | Success (x, _, _) -> Some x )

    let checkRepeatingProgram (p: Program) =
        let p' = p |> Map.map (fun _ v -> v,0)
        let update key = Map.change key (fun a -> 
            match a with
            | None -> None
            | Some (k,v) -> Some (k,v+1))

        let runInstruction acc pCounter = function
            | Acc x -> (acc + x), pCounter + 1
            | Jump x -> acc, (pCounter + x)
            | Nop _ -> acc, (pCounter + 1)
        let rec runProgram pCounter acc map = 
            let (op, iCounter) = Map.find pCounter map
            printfn "At instruction %s. reached this instruction %i time(s)" (toString op) iCounter
            if iCounter = 1 then acc else
            let (acc', pCounter') = runInstruction acc pCounter op
            let map' = update pCounter map
            runProgram pCounter' acc' map'
        runProgram 0 0 p'





        
    



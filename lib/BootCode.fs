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
        let update key value map = map |> Map.change key (fun x -> 
            match x with
            | None -> None
            | Some _ -> Some value)
        let runInstruction acc pCounter = function
            | Acc x -> (acc + x), pCounter + 1
            | Jump x -> acc, (pCounter + x)
            | Nop _ -> acc, (pCounter + 1)
        
        let opToString = function
        | Acc x -> sprintf "acc %i" x
        | Jump x -> sprintf "jmp %i" x
        | Nop x -> sprintf "nop %i" x

        let rec runProgramToRepeat acc pCounter history (map: Program)= 
            if (Map.count map <= pCounter) then 
                Result.Ok acc
            else 
                let op = Map.find pCounter map
                let repeat = history
                            |> List.map fst
                            |> List.contains pCounter
                if repeat then
                    Result.Error history
                else 
                    let h = [pCounter,op] @ history
                    let acc',c' = runInstruction acc pCounter op
                    runProgramToRepeat acc' c' h map

        let rp = runProgramToRepeat 0 0 []
        let firstrun = 
            match (rp p) with
            | Result.Error x -> x
            | Result.Ok _ -> raise (System.Exception())
        let flipOp = function
        | Acc x -> Acc x
        | Jump x -> Nop x
        | Nop x -> Jump x

        let isSuccess = function
        | Result.Error _ -> false
        | Result.Ok _ -> true

        let getResult = function
        | Result.Ok x -> x
        | _ -> raise (System.Exception ())

        firstrun
        |> List.filter (fun (_,x) -> match x with
                                        | Acc _ -> false
                                        | _ -> true )
        |> List.map (fun (i,op) -> update i (flipOp op) p |> rp)
        |> List.filter isSuccess
        |> List.exactlyOne
        |> getResult

        





        
    



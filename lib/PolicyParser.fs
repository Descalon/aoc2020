namespace Lib
open FParsec

module PolicyParser =

    let private split (s:string) (c:char) = 
        let sp = s.Split(c,2)
        (sp.[0].Trim(), sp.[1].Trim())
    
    let parse s = 
        let (policy, input) = split s ':'
        let policyParser = tuple3 pint32 (pstring "-" >>. pint32 .>> spaces) anyChar
        let result = run policyParser policy
        match result with
        | Failure (s,_,_) -> Result.Error s
        | Success ((min,max,expected), _, _) ->
            let c = (Seq.filter((=) expected) >> Seq.length) input
            match c with
            | x when (min <= x && x <= max) -> Result.Ok ()
            | _ -> Result.Error "Invalid Password" 
    
    let parse' s =
        let (policy, input) = split s ':'
        let policyParser = tuple3 pint32 (pstring "-" >>. pint32 .>> spaces) anyChar
        let result = run policyParser policy
        match result with
        | Failure (s,_,_) -> Result.Error s
        | Success ((min,max,expected), _, _) ->
            let a  = input.[min-1] = expected
            let b = input.[max-1] = expected
            if a <> b then
                Result.Ok ()
            else 
                Result.Error "Invalid Password"
namespace Lib

open FParsec

module Luggage =
    type RuleValue = {
        Key: string
        Value: int
    }
    type Rule = {
        Key: string
        Values: seq<RuleValue>
    }
    let private createRule (k,vs) = 
        let rv = vs |> List.map (fun (n,v) -> {Key = v; Value = n})
        { Key = k; Values = rv }

    let private pword = many1Satisfy isAsciiLetter

    let bagParser = pipe2 pword ((spaces >>. pword .>> spaces) .>> (pstring "bags" <|> pstring "bag")) 
                            (fun x y -> (sprintf "%s %s" x y))

    let contentsParser =
        pint32 .>>. (spaces >>. bagParser)

    let lineParser =
        bagParser .>>. (spaces >>. pstring "contain" >>. spaces >>. sepBy1 contentsParser (pstring ", "))

    let x=  run lineParser "foo"

    let private processResult = function
    | Failure (f,_,_) -> Seq.empty
    | Success (kv,_,_) -> seq {yield createRule kv}

    let parseRuleset (input: string list) =
        input
        |> List.map ((run lineParser) >> processResult)
        |> Seq.concat
    
    let hasValue (r:Rule) k =
        r.Values
        |> Seq.exists (fun x -> x.Key = k)
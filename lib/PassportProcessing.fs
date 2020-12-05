namespace Lib

open FParsec

module PassportProcessing =
    type PData =  
        | BirthYear         of int
        | IssueYear         of int
        | ExpirationYear    of int
        | Height            of int
        | HairColour        of string
        | EyeColour         of string
        | Id                of string
        | Country           of int

    let pColour = pchar '#' >>. manyChars hex >>= (fun x -> if x.Length = 6 then preturn x else fail "Colour is missing digits")
    let pSep = pchar ';'
    let pint o = pint32 |>> o

    let ParseEyeColour = 
        let s = pstring
        choice [
            s "amb"
            s "blu"
            s "brn"
            s "gry"
            s "grn"
            s "hzl"
            s "oth"
        ]

    let inRange min max input = 
        min <= input && input <= max
    
    let pByr    = pstring "byr:" >>. pint32
                                        >>= (fun x -> if (inRange 1920 2002 x) then preturn x else fail "Birth year out of range" ) |>> BirthYear
    let pIyr    = pstring "iyr:" >>. pint32
                                        >>= (fun x -> if (inRange 2010 2020 x) then preturn x else fail "Issue year out of range" ) |>> IssueYear
    let pEyr    = pstring "eyr:" >>. pint32
                                        >>= (fun x -> if (inRange 2020 2030 x) then preturn x else fail "Expiration year out of range" ) |>> ExpirationYear
    let pHgt    = pstring "hgt:" >>. pint32 .>>. ( pstring "cm" <|> pstring "in") 
                                                                                >>= (fun (x,y) -> 
                                                                                    match y with
                                                                                    | "in" -> if (inRange 59 76 x) then preturn x else fail "Height (in) out of range"
                                                                                    | "cm" -> if (inRange 150 193 x) then preturn x else fail "Height (cm) out of range"
                                                                                    | _ -> fail "What measure are you using?") |>> Height
    let pHcl    = pstring "hcl:" >>. pColour |>> HairColour
    let pEcl    = pstring "ecl:" >>. ParseEyeColour |>> EyeColour
    let pId     = pstring "pid:" >>. anyString 9 |>> Id
    let pCid    = pstring "cid:" >>. pint Country

    let pvalue, pvalueRef = createParserForwardedToRef<PData, unit>()

    let value = pvalue .>> pSep

    do pvalueRef := choice [pByr
                            pIyr
                            pEyr
                            pHgt
                            pHcl
                            pEcl
                            pId
                            pCid]

    let passport = many value .>> eof

    let validate = function
    | BirthYear _ -> true       
    | IssueYear _ -> true       
    | ExpirationYear _ -> true  
    | Height _ -> true          
    | HairColour _ -> true      
    | EyeColour _ -> true       
    | Id _ -> true              
    | _     -> false         

    let isValid p =
        let rec fn acc = function
        | [] -> acc
        | (x::xs) ->
            let acc' = if validate x then acc + 1 else acc
            fn acc' xs
        (fn 0 p) >= 7

    let checkPassport (input: string list) = 
        let rec fn = function
        | [] -> []
        | (x::xs) ->
            let n = run passport x
            match n with 
            | Failure (x,_,_) -> []
            | Success (ys, _, _) ->
                ys @ fn xs
        let result = fn input
        if List.isEmpty result then false else isValid result
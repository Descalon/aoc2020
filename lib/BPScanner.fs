namespace Lib

module BPScanner =

    type SeatInfo = {Row: int; Column: int; Id: int}

    let rec calcBinPart cMin cMax min max = function
    | [] -> 0
    | ([x]) ->
        match x with
        | _ when x = cMin -> min
        | _ when x = cMax -> max
        | _ -> raise(System.ArgumentException())
    | (x::xs) ->
        let pivot = (max - min) / 2
        match x with
        | _ when x = cMin -> 
            let max' = min + pivot
            calcBinPart cMin cMax min max' xs
        | _ when x = cMax ->
            let min' = max - pivot
            calcBinPart cMin cMax min' max xs 
        | _ -> raise (System.ArgumentException ())

    let calculateRow = calcBinPart 'F' 'B'
    let calculateColumn = calcBinPart 'L' 'R'

    let calcId row column = row * 8 + column
    let calc (row, column) = calcId row column

    let calculateSeat (input:string) = 
        let row = Seq.take 7 input |> Seq.toList |> calculateRow 0 127
        let column = Seq.skip 7 input |> Seq.toList |> calculateColumn 0 7
        {
            Row = row; Column = column; Id = calcId row column
        }
module ScannerTests

open Xunit;
open Lib.BPScanner;

let assertEqualInt (a:int) (b:int) = Assert.Equal(a,b)

let assertSeatInfo row col id (seat:Lib.BPScanner.SeatInfo) = 
    Assert.Equal(row, seat.Row)
    Assert.Equal(col, seat.Column)
    Assert.Equal(id, seat.Id)


[<Theory>]
[<InlineData("FBFBBFF", 44)>]
[<InlineData("FFFBBBF", 14)>]
[<InlineData("BBFFBBF", 102)>]
let ``Row Test`` (input, expected) =
    Lib.BPScanner.calculateRow 0 127 (input |> Seq.toList)
    |> assertEqualInt expected

[<Theory>]
[<InlineData("RRR", 7)>]
[<InlineData("RLL", 4)>]
let ``Column Test`` (input, expected) =
    Lib.BPScanner.calculateColumn 0 7 (input |> Seq.toList)
    |> assertEqualInt expected

[<Theory>]
[<InlineData("FBFBBFFRLR", 44, 5, 357)>]
[<InlineData("BFFFBBFRRR", 70, 7, 567)>]
[<InlineData("FFFBBBFRRR", 14, 7, 119)>]
[<InlineData("BBFFBBFRLL", 102, 4, 820)>]
let ``Full seat test`` (input, eRow, eCol, eId) =
    Lib.BPScanner.calculateSeat input
    |> assertSeatInfo eRow eCol eId

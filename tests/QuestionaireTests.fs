module QuestionaireTests

open Xunit
open Lib.Questionaire

let assertInt (a:int) (b:int) = Assert.Equal(a,b)

[<Fact>]
let ``Intersection Test`` () =
    ["ab"; "ac"]
    |> union
    |> Set.count
    |> assertInt 3

[<Fact>]
let ``List of list Intersection Test``() =
    [["abc"]; ["a"; "b"; "c"]; ["ab"; "ac"]; ["a";"a";"a";"a"]; ["b"]]
    |> List.map union
    |> List.sumBy (Set.count)
    |> assertInt 11

[<Fact>]
let ``Crossreference test`` () =
    let g1 = ["c";"jc";"ck";"cue";"c";]
    let g2 = ["pt";"tp";"xtzn";"tu";"pt";]
    let c = [g1;g2]
    let count = union >> Set.count
    let r1 = count g1
    let r2 = count g2
    c
    |> List.map union
    |> List.sumBy(Set.count)
    |> assertInt (r1 + r2)
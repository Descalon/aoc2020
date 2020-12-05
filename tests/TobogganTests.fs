module TobogganTests

open Xunit
open Lib.Toboggan

let data = ["..##.......";"#...#...#..";".#....#..#.";"..#.#...#.#";".#...##..#.";"..#.##.....";".#.#.#....#";".#........#";"#.##...#...";"#...##....#";".#..#...#.#";]

let l = data.[0].Length 
let w = wrap l
let wp = wrapPoint l 


let aEqual (expected: uint) (actual:uint) = Assert.Equal(expected, actual)
let flip fn a b = fn b a

[<Fact>]
let ``Test with example data`` () =
    let result = processToboggan (3,1) data
    Assert.Equal(7, result)

[<Fact>]
let ``Test for part two``() =
    data
    |> processToboggan' [(1,1); (3,1); (5,1); (7,1); (1,2)]
    |> aEqual 336u

[<Fact>]
let ``Wrapping test`` () =
    Assert.Equal(0, w 11)
    let (x,y) = (10,2)
    let (x', y') = move (x,y) (3,1) |> wp
    Assert.Equal(2, x')
    Assert.Equal(3, y')
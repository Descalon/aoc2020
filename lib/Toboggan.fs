namespace Lib

module Toboggan =
    type Point = int * int

    let wrap max input = input % max
    let wrapPoint max (p:Point)= 
        let (x,y) = p
        Point (wrap max x, y)

    let move (m:Point) (pos:Point) =
        let (x,y) = pos
        let (x', y') = m
        Point (x'+x, y'+y)
    
    let check (line : string) position = 
        line.[position] = '#'

    let processToboggan (movement:Point) (data: string list) = 
        let l = data.[0].Length 
        let w = wrap l
        let wp = wrapPoint l 
        let start = Point (0,0)
        let target = List.length data
        let rec fn pos acc = 
            let move' p = move p movement |> wp
            let (x,y) = move' pos
            if y >= target then acc else
                let check' = check data.[y]
                let acc' = if (check' x) then acc + 1 else acc
                let foo = if (check' x) then "x" else "o"
                fn (x,y) acc'
        fn start 0
    
    let processToboggan' (movements:Point list) (data: string list) = 
        let p m = processToboggan m data
        movements
        |> List.map (p >> uint32)
        |> List.fold (*) 1u

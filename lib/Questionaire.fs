namespace Lib


module Questionaire =
    type Group = string list

    let union (input: Group) = 
        input
        |> List.fold (fun s l -> Set.union s (Set.ofSeq l)) (Set.ofSeq (List.head input))
    
    let intersection (input: Group) = 
        input
        |> List.fold (fun s l -> Set.intersect s (Set.ofSeq l)) (Set.ofSeq (List.head input))

    
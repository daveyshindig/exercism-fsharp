module Matrix

let fromString (str: string) = 
    Seq.map (fun (s: string) -> s.Split(' ')) (str.Split('\n')) 
    |> Seq.toList
    |> List.map (fun row -> Array.map int row)

let rows matrix = matrix

let cols (matrix: int [] list) = 
    Array.mapi (fun i _ -> List.map (fun (arr: int []) -> arr.[i]) matrix) (List.head matrix)
    |> Array.toList
    |> List.map List.toArray
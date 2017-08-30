module Series

let charDigitMap = [('0', 0); ('1', 1); ('2', 2); ('3', 3); ('4', 4); 
                    ('5', 5); ('6', 6); ('7', 7); ('8', 8); ('9', 9)] |> Map.ofList

let slices (str: string) n = 
    let arr = Seq.toArray str |> Array.map (fun x -> Map.find x charDigitMap)
    if n > Array.length arr then failwith "N IS TOO LONGGGGGG"
    else Array.mapi (fun i _ -> arr.[i .. i + n - 1] |> Array.toList) arr.[0  .. Array.length arr - n]
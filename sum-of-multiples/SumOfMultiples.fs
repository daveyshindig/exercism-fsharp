module SumOfMultiples

let sumOfMultiples (xs : int list) (limit : int) : int =
    let rec getMults xs ls =
        match ls with 
        | [] -> []
        | hd :: tl when List.exists (fun x -> hd % x = 0) xs -> 
            hd :: getMults xs tl
        | hd :: tl -> getMults xs tl
        | [hd] when List.exists (fun x -> hd % x = 0) xs -> [hd]
        | [_] -> []

    if limit > 0 then
        let mults = getMults xs [1..limit-1]
        List.sum mults
    else 0
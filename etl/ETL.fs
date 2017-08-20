module ETL

let transform (old: Map<int, string list>) = 
    let getChars (chars: string list option) = 
        match chars with
        | Some ls -> ls
        | None -> []
    List.collect (fun i -> List.map (fun (c: string) -> (c.ToLower(), i)) (getChars (Map.tryFind i old)))
                 [1 .. 10]
    |> Map.ofList
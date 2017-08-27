module SecretHandshake

let rec intToBinary x =
    match x with
    | 0 | 1 -> string x
    | _ -> intToBinary (x / 2) + string (x % 2)

let handshake (x: int) =
    let strs = ["wink"; "double blink"; "close your eyes"; "jump"; "reverse"]
    let nums = List.rev (List.ofSeq (intToBinary x))
    let strs' = List.mapi (fun i x -> if x = '1' then strs.[i] else "") nums
    if strs'.Length = 5 then List.tail (List.rev strs') else strs'
    |> List.filter (fun str -> str <> "")
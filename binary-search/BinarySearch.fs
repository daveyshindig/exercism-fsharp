module BinarySearch

let rec binarySearch (arr: int array) (x: int) =
    if arr.Length = 0 then None else
    let rec binarySearch' (arr: int array) x i = 
        match arr.[arr.Length / 2] with
        | mid when mid = x -> Some (i + arr.Length / 2)
        | _ when arr.Length = 1 -> None
        | mid when mid > x -> binarySearch' arr.[0 .. arr.Length / 2 - 1] x i
        | mid when mid < x -> binarySearch' arr.[arr.Length / 2 + 1 ..] x (i + arr.Length / 2 + 1)
        | _ -> failwith "binarySearch failed to match"
    binarySearch' arr x 0
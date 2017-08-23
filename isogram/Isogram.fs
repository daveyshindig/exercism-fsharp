module Isogram

let isogram (str: string) =
    let ls = List.filter System.Char.IsLetter (List.ofSeq (str.ToLower()))
    let rec isogram' (chars: char list) letters = 
        match chars with
        | [] -> true
        | c :: tl when List.exists (fun x -> x = c) letters -> false
        | c :: tl -> isogram' tl (c :: letters)
    isogram' ls []
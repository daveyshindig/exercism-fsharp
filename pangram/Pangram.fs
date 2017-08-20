module Pangram

let isPangram (str : string) =
    let alphabet = Set.ofList ['a' .. 'z']
    let letters = str.ToLower() |> Set.ofSeq |> Set.filter System.Char.IsLetter
    Set.difference alphabet letters |> Set.isEmpty
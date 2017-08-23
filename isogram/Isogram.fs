module Isogram
open System

let isogram (str: string) =
    str
    |> Seq.filter Char.IsLetter
    |> Seq.map Char.ToLowerInvariant
    |> Seq.countBy id
    |> Seq.forall (fun (_,x) -> x = 1)

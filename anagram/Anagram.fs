module Anagram
open System

let anagram (s1: string) (s2: string) =
    (Seq.countBy id (s1.ToLower()) |> Map.ofSeq) = (Seq.countBy id (s2.ToLower()) |> Map.ofSeq)

let anagrams words (word: string) = 
    let words' = List.filter (fun (w: string) -> w.ToLower() <> word.ToLower()) words
    List.filter (anagram word) words'
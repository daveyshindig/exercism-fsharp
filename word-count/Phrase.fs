module Phrase
open System
open System.Text.RegularExpressions

let wordCount (phrase: string) =
    let rec wc words map =
        match words with
        | [] -> map
        | word :: tl when Map.containsKey word map -> 
            let n = Map.find word map
            wc tl (Map.add word (n+1) map)
        | word :: tl -> wc tl (Map.add word 1 map)
    let phrase' = Regex.Replace(phrase.ToLower(), "[.,!@#$%^&*:]| +'+|'$", " ")
    let phrase'' = Regex.Replace(phrase', "\\s+", " ") 
    let words = List.ofArray (phrase''.Split(' '))
    wc words Map.empty |> Map.remove "" 
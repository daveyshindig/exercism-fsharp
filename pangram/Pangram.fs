module Pangram

let isPangram (str : string) =
    let alphabet = Set.ofList ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 
        'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 
        'x'; 'y'; 'z';]
    let chars = Seq.distinct (str.ToLower()) |> Set.ofSeq
    (Set.filter (fun c -> Set.contains c alphabet) chars |> Set.count) = 26
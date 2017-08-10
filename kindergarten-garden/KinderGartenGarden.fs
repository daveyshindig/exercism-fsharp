module KinderGartenGarden

type Plant =
    | Radishes
    | Clover
    | Grass
    | Violets

let rec plantListOfChars (chars: char list) : Plant list = 
    match chars with
    | [] -> []
    | c :: tl when c = 'C' -> Clover :: plantListOfChars tl
    | c :: tl when c = 'G' -> Grass :: plantListOfChars tl
    | c :: tl when c = 'R' -> Radishes :: plantListOfChars tl
    | c :: tl when c = 'V' -> Violets :: plantListOfChars tl
    | _ -> []

let lookupPlants (name: string) (garden: Map<string, Plant list>) : Plant list =
    match Map.tryFind name garden with 
    | Some x -> x
    | None -> []

let garden (names: string list) (str: string) : Map<string, Plant list> = 
    let explode (s: string) = [for c in s -> c]
    let rows = str.Split [|'\n'|]
    let row1 = explode rows.[0] |> plantListOfChars |> Array.ofList
    let row2 = explode rows.[1] |> plantListOfChars |> Array.ofList
    let rec makeGarden (map: Map<string, Plant list>) (names: string list) (r1: Plant array) (r2: Plant array) = 
        match names with
        | name :: tl when Array.length r1 > 2-> 
            let map' = Map.add name [r1.[0]; r1.[1]; r2.[0]; r2.[1]] map
            makeGarden map' tl r1.[2..] r2.[2..]
        | name :: tl -> 
            let map' = Map.add name [r1.[0]; r1.[1]; r2.[0]; r2.[1]] map
            map'
        | [] -> map
    makeGarden Map.empty (List.sort names) row1 row2

let defaultGarden (str: string) : Map<string, Plant list> = 
    let names = ["Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; 
                 "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry"]
    garden names str
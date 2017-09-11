module AllYourBase

open System 

let rebase inBase inDigits outBase = 
    let rec base10toOut outBase acc digits = 
        if digits = 0 then acc
        else base10toOut outBase ((digits % outBase) :: acc) (digits / outBase)
    if inBase < 2 || outBase < 2 || List.isEmpty inDigits || 
       List.exists (fun x -> x < 0 || x >= inBase) inDigits
    then None
    else
        let ls = inDigits
                |> List.rev
                |> List.mapi (fun i x -> float x * Math.Pow(float inBase, float i))
                |> List.sum
                |> int
                |> base10toOut outBase []
        if List.isEmpty ls then Some [0] else Some ls
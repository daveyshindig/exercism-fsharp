module PerfectNumbers

type Number = Perfect | Deficient | Abundant

let classify x =
    let factors = [for n in [1 .. x/2] do if x % n = 0 then yield n]
    match List.sum factors with
    | n when n < x -> Deficient
    | n when n = x -> Perfect
    | n when n > x -> Abundant
    | _ -> failwith "Failure to classify perfect/deficient/abundant number"
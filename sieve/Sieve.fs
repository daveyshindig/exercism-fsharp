module Sieve

let primesUpTo limit =
    let range =  [2 .. limit]
    let rec sift candidates primes =
        match candidates with 
        | [] -> List.rev primes
        | hd :: tl -> sift (List.filter (fun x -> x % hd <> 0) tl) (hd :: primes)
    sift range []
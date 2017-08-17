module Hamming

let compute s1 s2 =
    Seq.map2 (fun c1 c2 -> if c1 = c2 then 0 else 1) s1 s2 |> Seq.sum
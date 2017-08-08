module Raindrops

let convert (x : int) = 
    let factors = [ (3, "Pling"); (5, "Plang"); (7, "Plong"); ]
    let str = String.concat "" (seq { for i in factors do if x % fst i = 0 then yield snd i })
    if str = "" then string x else str
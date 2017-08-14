module PigLatin

let translate (str: string) = 
    let words = str.Split ' '
    let trans (word: string) =
        if word.[0..2] = "thr" || word.[0..2] = "sch" 
        then word.[3..] + word.[0..2] + "ay"
        elif word.[1..2] = "qu"
        then word.[3..] + word.[0..2] + "ay"
        elif word.[0..1] = "yt" || word.[0..1] = "xr"
        then word + "ay"
        elif word.[0..1] = "ch" || word.[0..1] = "qu" || word.[0..1] = "th" 
        then word.[2..] + word.[0..1] + "ay"
        else match word.[0] with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> word + "ay"
        | _ -> word.[1..] + string word.[0] + "ay" 
    Array.reduce (fun str1 str2 -> str1 + " " + str2) (Array.map trans words)
module Bob
open System.Text.RegularExpressions

let isQuestion (str : string) : bool = 
    str.[str.Length-1] = '?'

let isShouting (str : string) : bool = 
    let pattern = @"^[0-9A-Z\p{P}\p{S}\ ]*$"
    let rgx = new Regex(pattern)
    rgx.IsMatch(str)

let isNumberQuestion (str : string) : bool = 
    let pattern = @"^[0-9\p{P}\p{S}\ ]*[?]+$"
    let rgx = new Regex(pattern)
    rgx.IsMatch(str)

let isOnlyNumbers (str : string) : bool = 
    let pattern = @"^[0-9\p{P}\p{S}\ ]*$"
    let rgx = new Regex(pattern)
    rgx.IsMatch(str)

let hey (str : string) : string = 
    match str.Trim() with
    | "" -> "Fine. Be that way!"
    | _ when isNumberQuestion str -> "Sure."
    | _ when isOnlyNumbers str -> "Whatever."
    | _ when isShouting str -> "Whoa, chill out!"
    | _ when isQuestion str -> "Sure."
    | _ -> "Whatever."



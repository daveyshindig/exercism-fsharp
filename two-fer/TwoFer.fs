module TwoFer

let getResponse name =
    match name with
    | Some str -> sprintf "One for %s, one for me." str
    | None -> "One for you, one for me."
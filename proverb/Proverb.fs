module Proverb

let nouns = ["nail"; "shoe"; "horse"; "rider"; "message"; "battle"; "kingdom"]

let line x = 
    match x with
    | i when i > 0 && i < 7 -> "For want of a " + nouns.[i-1] + " the " 
                                + nouns.[i] + " was lost."
    | i when i = 7 -> "And all for the want of a horseshoe nail."
    | _ -> failwith "For want of a line number this song was lost."

let proverb = 
    let lines = List.map line [1 .. 7]
    List.reduce (fun ln1 ln2 -> ln1 + "\n" + ln2) lines
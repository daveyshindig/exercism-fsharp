module Accumulate

let accumulate (f : 'a -> 'a) (ls : 'a list) : 'a list =
    let rec accumulate' f ls acc =
        match ls with 
        | [] -> List.rev acc
        | hd :: tl -> accumulate' f tl (f hd :: acc)
    accumulate' f ls []
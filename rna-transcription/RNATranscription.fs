module RNATranscription

let toRna str = 
    let transcribe c = 
        match c with
        | 'C' -> 'G'
        | 'G' -> 'C'
        | 'T' -> 'A'
        | 'A' -> 'U'
        | _ -> failwith "Invalid DNA sequence"
    System.String.Concat [for c in str do yield transcribe c]
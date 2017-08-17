module RNATranscription

let toRna str = 
    let complement = ['C', 'G'; 'G', 'C'; 'T', 'A'; 'A', 'U'] |> Map.ofList
    String.map (fun c -> complement.[c]) str
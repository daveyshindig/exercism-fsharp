module NucleoTideCount

let nucleotideCounts strand = 
    let map = Seq.countBy id strand |> Map.ofSeq
    let maybeAdd map key = 
        if Map.containsKey key map then map
        else Map.add key 0 map
    List.fold maybeAdd map ['A'; 'T'; 'C'; 'G';]

let count c strand = 
    if not (List.exists (fun nucleotide -> nucleotide = c) ['A'; 'T'; 'C'; 'G';])
    then raise (System.Exception "Invalid nucleotide")
    else Map.find c (nucleotideCounts strand)
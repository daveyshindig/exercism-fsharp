module ProteinTranslation

let translate str = 
    let rec translate' (str: string) ls = 
        try
            match str.[0..2] with
            | "AUG" -> translate' str.[3..] ("Methionine" :: ls)
            | "UUU" | "UUC" -> translate' str.[3..] ("Phenylalanine" :: ls)
            | "UUA" | "UUG" -> translate' str.[3..] ("Leucine" :: ls)
            | "UCU" | "UCC" | "UCA" | "UCG" -> translate' str.[3..] ("Serine" :: ls)
            | "UAU" | "UAC" -> translate' str.[3..] ("Tyrosine" :: ls)
            | "UGU" | "UGC" -> translate' str.[3..] ("Cysteine" :: ls)
            | "UGG" ->  translate' str.[3..] ("Tryptophan" :: ls)
            | "UAA" | "UAG" | "UGA" -> ls
            | _ -> failwith "Codon not recognized"
        with :? System.ArgumentOutOfRangeException -> ls
    List.rev (translate' str [])
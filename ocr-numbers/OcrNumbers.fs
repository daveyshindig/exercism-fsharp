module OcrNumbers

let rec convert (str: string) = 
    let strs = str.Split '\n'
    let convertSingleDigit ls = 
        match ls with
        | " _ " :: "| |" :: "|_|" :: "   " :: _ -> "0"
        | "   " :: "  |" :: "  |" :: "   " :: _ -> "1"
        | " _ " :: " _|" :: "|_ " :: "   " :: _ -> "2"
        | " _ " :: " _|" :: " _|" :: "   " :: _ -> "3"
        | "   " :: "|_|" :: "  |" :: "   " :: _ -> "4"
        | " _ " :: "|_ " :: " _|" :: "   " :: _ -> "5"
        | " _ " :: "|_ " :: "|_|" :: "   " :: _ -> "6"
        | " _ " :: "  |" :: "  |" :: "   " :: _ -> "7"
        | " _ " :: "|_|" :: "|_|" :: "   " :: _ -> "8"
        | " _ " :: "|_|" :: " _|" :: "   " :: _ -> "9"
        | _ -> "?"
    let rec convertMultipleDigits (ls: string list) : string =
        if String.length ls.[0] > 0 then 
            let digit = List.map (fun (s: string) -> s.[0..2]) ls
            if String.length ls.[0] > 3 then
                let moreDigits = List.map (fun (s: string) -> s.[3..]) ls
                convertSingleDigit digit +  convertMultipleDigits moreDigits
            else convertSingleDigit digit
        else ""
    let rec convertMultipleLines (ls: string list) : string = 
        let len = List.length ls
        match ls with 
        | a :: b :: c :: d :: tl when len > 4 -> convertMultipleDigits [a; b; c; d] +
                                                 "," + convertMultipleLines tl
        | a :: b :: c :: d :: tl when len > 0 -> convertMultipleDigits [a; b; c; d]
        | _ -> ""
    Array.toList strs |> convertMultipleLines
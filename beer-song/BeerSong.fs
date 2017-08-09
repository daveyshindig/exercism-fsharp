module BeerSong

let verse x = 
    let str s1 s2 s3 s4 = sprintf "%s of beer on the wall, %s of beer.\n\
                           %s, %s of beer on the wall.\n\n" s1 s2 s3 s4
    match x with
    | 0 -> str "No more bottles" "no more bottles" 
               "Go to the store and buy some more" "99 bottles"
    | 1 -> str (string 1 + " bottle") (string 1 + " bottle")
               "Take it down and pass it around" "no more bottles"
    | y when y > 1 -> 
        let lastbottle = if y - 1 = 1 then " bottle" else " bottles"
        str (string y + " bottles") (string y + " bottles") 
             "Take one down and pass it around" (string (y - 1) + lastbottle)
    | _ -> ""

let rec verses x y =
    match x - y with
    | 0 -> verse y
    | _ -> verse x + verses (x - 1) y

let sing = verses 99 0
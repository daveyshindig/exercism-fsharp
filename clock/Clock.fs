module Clock

let mkClock h m = (h % 24 + m / 60, m % 60)

let display clock = sprintf "%02i:%02i" (fst clock) (snd clock)

let add m clock = 
    let h = (fst clock + m / 60 ) % 24
    let m = (snd clock + m % 60) % 60
    let overflow = if snd clock + m % 60 > 59 then 1 else 0
    (h + overflow) % 24, m

let subtract m clock =
    let m' = snd clock - m % 60
    let overflow = if m' < 0 then 1 else 0
    let m'' = if m' < 0 then m' + 60 else m'
    let h' = fst clock - m / 60 - overflow
    let h'' = if h' < 0 then h' + 24 else h'
    h'', m''
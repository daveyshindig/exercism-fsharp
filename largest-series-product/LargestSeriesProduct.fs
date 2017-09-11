module LargestSeriesProduct
open System

let largestProduct (input: string) (seriesLength: int) : int =
    if seriesLength = 0 then 1
    elif input.Length = 0 then failwith "Cannot slice empty string with nonzero span"
    else
    let arr = Seq.map string input |> Seq.toArray
    let i = Array.length arr - seriesLength
    let intArr = Array.map System.Int32.Parse arr
    Array.mapi (fun j _ -> Array.reduce (*) intArr.[j..j+seriesLength-1]) intArr.[0..i]
    |> Array.max

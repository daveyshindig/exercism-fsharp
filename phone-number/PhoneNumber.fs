module PhoneNumber
open System

let parsePhoneNumber str = 
    let digits = Seq.filter Char.IsDigit str
    if Seq.length digits = 7 || Seq.length digits = 10
    then Some (String.Concat digits) 
    elif Seq.length digits = 11 && Seq.head digits = '1'
    then Some (String.Concat (Seq.skip 1 digits)) 
    else None
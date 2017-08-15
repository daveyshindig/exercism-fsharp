module Gigasecond

let gigasecond (date: System.DateTime) : System.DateTime =
    date.Add(System.TimeSpan.FromSeconds(1000000000.)).Date
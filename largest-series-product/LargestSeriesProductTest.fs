module LargestSeriesProductTest

open NUnit.Framework
open LargestSeriesProduct
    
[<TestCase("01234567890", 2, ExpectedResult = 72)>]
[<TestCase("1027839564", 3, ExpectedResult = 270)>]
let ``Gets the largest product``(digits: string) (seriesLength: int) =
    largestProduct digits seriesLength

    
[<TestCase("19", 2, ExpectedResult = 9)>]
let ``Largest product works for small numbers`` (digits: string) (seriesLength: int) =
    largestProduct digits seriesLength
    
[<TestCase ("73167176531330624919225119674426574742355349194934", 6, ExpectedResult = 23520)>]
let ``Largest product works for large numbers`` (digits: string) (seriesLength: int)  =
    largestProduct digits seriesLength 
    
[<TestCase("0000", 2, ExpectedResult = 0)>]
[<TestCase("99099", 3, ExpectedResult = 0)>]
let ``Largest product works if all spans contain zero``(digits: string) (seriesLength: int) =
    largestProduct digits seriesLength

    
[<TestCase("", 0, ExpectedResult = 1)>]
[<TestCase("123", 0, ExpectedResult = 1)>]
let ``Largest product for empty span is 1``(digits: string) (seriesLength: int) =
    largestProduct digits seriesLength

    
[<TestCase("", 1)>]
let ``Cannot slice empty string with nonzero span`` (digits: string) (seriesLength: int) =
    Assert.That((fun () -> largestProduct digits seriesLength |> ignore), Throws.Exception)

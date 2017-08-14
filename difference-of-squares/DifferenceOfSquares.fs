module DifferenceOfSquares

let square x = pown x 2

let squareOfSums n = List.sum [1..n] |> square

let sumOfSquares n = List.sumBy square [1..n]

let difference x = System.Math.Abs (sumOfSquares x - squareOfSums x)
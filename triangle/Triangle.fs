module Triangle

type TriangleKind = Equilateral | Isosceles | Scalene

let kind x y z = 
    if x <= 0m || y <= 0m || z <= 0m 
    then raise (System.InvalidOperationException "Triangle side <= 0")
    elif x > y + z || y > x + z || z > x + y
    then raise (System.InvalidOperationException "Triangle side <= 0")
    elif x = y && y = z then Equilateral
    elif x = y || y = z || x = z then Isosceles
    else Scalene
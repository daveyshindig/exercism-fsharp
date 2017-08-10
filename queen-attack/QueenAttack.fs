module QueenAttack

let canAttack ((wx, wy): int * int) ((bx, by): int * int) =  
    if wx = bx && wy = by then raise (System.FormatException "Queens cannot occupy same space")
    else wx = bx || wy = by || System.Math.Abs (wx - bx) = System.Math.Abs (wy - by)    
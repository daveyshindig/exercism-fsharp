module BinarySearchTree

type BinarySearchTree = BinarySearchTree of int * BinarySearchTree option * BinarySearchTree option

let singleton x = BinarySearchTree (x, None, None)
let value (BinarySearchTree (x, _, _): BinarySearchTree) = Some x
let left (BinarySearchTree (_, l, _): BinarySearchTree) = l 
let right (BinarySearchTree (_, _, r): BinarySearchTree) = r

let rec insert x (BinarySearchTree (y, l, r): BinarySearchTree) = 
    if x <= y then 
         match l with 
         | Some subTree -> BinarySearchTree (y, Some (insert x subTree), r)
         | None -> BinarySearchTree (y, Some (BinarySearchTree (x, None, None)), r)
    else match r with
         | Some subTree -> BinarySearchTree (y, l, Some (insert x subTree))
         | None -> BinarySearchTree (y, l, Some (BinarySearchTree (x, None, None)))

let rec fromList (list: int list) = 
    let rec helper ls tree = 
        match ls with 
        | [] -> tree
        | x :: tl -> helper tl (insert x tree)
    helper (List.tail list) (singleton list.[0])

let rec toList (tree: BinarySearchTree) =
    let rec helper (BinarySearchTree (x, l, r): BinarySearchTree) =
        match l, r with
        | None, None -> [x]
        | Some tree, None -> x :: toList tree
        | None, Some tree -> x :: toList tree
        | Some tree1, Some tree2 -> x :: toList tree1 @ toList tree2
    helper tree |> List.sort
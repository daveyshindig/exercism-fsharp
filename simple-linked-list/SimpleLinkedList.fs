module SimpleLinkedList

type SinglyLinkedList<'T> =
    | Nil
    | ListNode of 'T * SinglyLinkedList<'T>

let nil = Nil

let isNil sll = sll = Nil

let create t ls = ListNode (t, ls)

let datum (ls: SinglyLinkedList<'T>) =
    match ls with
    | Nil -> failwith "NO DATA"
    | ListNode (t, _) -> t

let next (ls: SinglyLinkedList<'T>) =
    match ls with
    | Nil -> failwith "NO DATA"
    | ListNode (_, ls) -> ls

let toList ls =
    let rec toListRec ls acc = 
        match ls with
        | Nil -> acc
        | ListNode (t, ls) -> toListRec ls (t :: acc)
    toListRec ls [] |> List.rev

let fromList ls = List.foldBack create ls Nil

let reverse ls =
    let rec reverseRec ls acc = 
        match ls with
        | Nil -> acc
        | ListNode (t, ls) -> reverseRec ls (create t acc)
    reverseRec ls Nil

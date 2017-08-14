module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let recordId t = 
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t = 
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t = 
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let buildTree records = 
    let records' = List.sortBy (fun x -> x.RecordId) records

    if List.isEmpty records' then failwith "Empty input"
    let root = records'.[0]
    if (root.ParentId <> 0 || root.RecordId <> 0) then failwith "Root node is invalid"

    let rec makeLeaves rs prev leaves =
        match rs with
        | [] -> leaves
        | r :: tl when r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId) ->
            failwith "Nodes with invalid parents"
        | r :: tl when r.RecordId <> prev + 1 -> failwith "Nodes with invalid parents"
        | r :: tl when r.RecordId = 0 -> makeLeaves tl r.RecordId ((-1, r.RecordId) :: leaves)
        | r :: tl -> makeLeaves tl r.RecordId ((r.ParentId, r.RecordId) :: leaves)

    let leaves = makeLeaves records' -1 [] |> List.rev
    let root = leaves.[0]

    let rec makeGroups leaves map =
        match leaves with
        | [] -> Map.map (fun k t -> List.rev t) map
        | leaf :: tl when Map.containsKey (fst leaf) map -> 
            let children = snd leaf :: Map.find (fst leaf) map
            makeGroups tl (Map.add (fst leaf) children map)
        | leaf :: tl -> makeGroups tl (Map.add (fst leaf) [snd leaf] map)

    let map = makeGroups leaves Map.empty

    let rec helper key =
        match Map.containsKey key map with
        | true -> Branch (key, List.map helper (Map.find key map))
        | false -> Leaf key                    

    helper 0
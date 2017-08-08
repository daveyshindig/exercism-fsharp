module GradeSchool

let empty = Map.empty

let add (name : string) (grade : int) (school : Map<int,string list>) : Map<int,string list> =
    match Map.tryFind grade school with
    | None -> Map.add grade [name] school
    | Some ls -> Map.add grade (List.sort (name :: ls)) school

let grade (num : int) (school : Map<int, string list>) : string list =
    match Map.tryFind num school with
    | None -> []
    | Some ls -> ls

let roster (school : Map<int, string list>) : (int * string list) list =
    Map.toList school
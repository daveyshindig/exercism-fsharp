module ErrorHandling

type Result<'TSuccess, 'TError> = Ok of 'TSuccess | Error of 'TError

let handleErrorByThrowingException() =
    raise (System.Exception "UH OH")

let handleErrorByReturningOption<'T> (x: string) =
    try
        Some (int x)
    with 
    | :? System.FormatException -> None

let handleErrorByReturningResult<'TSuccess, 'TError> (x: string) =
    try
        Ok (int x)
    with 
    | :? System.FormatException -> Error "Could not convert input to integer"
    
let bind (f: int -> Result<int, _>) (r: Result<int, _>) : Result<int, _> =
    match r with
    | Ok x -> f x
    | Error _ -> r

let cleanupDisposablesWhenThrowingException (resource: System.IDisposable) =  
    try
        raise (System.Exception "ERROR AHHHH")
    finally
        resource.Dispose()

open System
open Expecto.ExpectoFsCheck
open FsCheck

let revOfRevIsOrig (x: int list) = 
    List.rev (List.rev x) = x

[<EntryPoint>]
let main argv =
    printfn "Testing with FSCheck"
    Check.Quick revOfRevIsOrig |> ignore
    Console.ReadKey() |> ignore // wait at end
    0 // return an integer exit code
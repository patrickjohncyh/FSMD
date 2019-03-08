open System

open TableTypes
open TableHandler
open TableHandlerTest

open Expecto
open FsCheck

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig [||] |> ignore 
    Console.ReadKey() |> ignore
    0 // return an integer exit code

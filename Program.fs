open System

open InlineParser
open InlineParserTest


open Expecto
open FsCheck

let expectoConfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}


[<EntryPoint>]
let main argv =
    // run all tests tagged with [<TESTS>] - can be in any file
    runTestsInAssembly expectoConfig [||] |> ignore // run all tests defined with [<Tests>]
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0
    
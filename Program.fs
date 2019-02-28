open System
open BlockParserTypes
open BlockParser
open BlockParserTests
open Expecto

let expectoConfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}
let allTestsWithExpecto() =
        runTestsInAssembly defaultConfig [||]
[<EntryPoint>]
let main argv =
    allTestsWithExpecto() |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
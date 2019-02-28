open System
open BlockParserTypes
open BlockParser
open BlockParserTests
open Expecto

let testsWithExpecto() =
        runTests defaultConfig testListWithExpecto |> ignore
let allTestsWithExpecto() =
        runTestsInAssembly defaultConfig [||]
[<EntryPoint>]
let main argv =
    allTestsWithExpecto() |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
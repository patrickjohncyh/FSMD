// Learn more about F# at http://fsharp.org
module FSMDTOP

open System
open System.IO
open Types
open InlineParser
open BlockParser
open TableHandler
open BlockHandlers
open BlockDispatcher

(*  
let fileStream (fPath:string) =
    try
        File.ReadAllLines(fPath) |> Ok
    with
    | :? System.ArgumentException           -> Error <| sprintf "Path is a zero-length string"
    | :? System.UnauthorizedAccessException -> Error <| sprintf "Non authorized access to file"
    | :? System.IO.FileNotFoundException    -> Error <| sprintf "Could not find the file %A" fPath

/// markdown test file
let testFilePath = @"markdown.txt"

/// convert string array into list array
let linesList = fileStream testFilePath
               |> function | Ok sList -> Some (Array.toList sList) | Error _ -> None
             
[<EntryPoint>]
let main argv =

    let lines = testFilePath
                |> fileStream
                |> function | Ok sList -> Some (Array.toList sList) | Error _ -> None
    lines
    |> blockParser
    |> blockDispatcher
    |> printfn "%A"
  
    0 // return an integer exit code
*)

let FSMDTop lines = 
    lines
    |> blockParser
    |> blockDispatcher

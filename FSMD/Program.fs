// Learn more about F# at http://fsharp.org
open System
open System.IO
open Types
open InlineParser
open BlockParser
open TableHandler
open BlockHandler



let blockDispatcher rawBlocks =
    let callBlockHandler rblock =
        match rblock.blocktype with
        | Para       -> rblock.mData |> paragraphBlockHandler
        | Heading1   -> rblock.mData |> h1BlockHandle
        | Heading2   -> rblock.mData |> h1BlockHandle
        | Heading3   -> rblock.mData |> h1BlockHandle
        | Heading4   -> rblock.mData |> h1BlockHandle
        | Heading5   -> rblock.mData |> h1BlockHandle
        | Heading6   -> rblock.mData |> h1BlockHandle
        | CBlock     -> rblock.mData |> codeBlockHandler
        | BlockQuote -> failwithf "What? Block dispatcher for BlockQuote not implemented"
        | List       -> failwithf "What? Block dispatcher for List not implemented"
        | TableBlock -> failwithf "What? Block dispatcher for Table not implemented"
        | _          -> []

    match rawBlocks with
    | Error x -> []
    | Ok (rbList,linkRefList) ->
        rbList |> List.collect callBlockHandler


[<EntryPoint>]
let main argv =

    Some ["# lol";"";"lol"] 
    |> blockParser 
    |> blockDispatcher
    |> printfn "%A"
    0 // return an integer exit code

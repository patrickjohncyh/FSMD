// Learn more about F# at http://fsharp.org
open System

open Types
open InlineParser
open BlockParser
open TableHandler
open BlockHandler



let blockDispatcher rawBlocks =
    let callBlockHandler rblock =
        match rblock.blocktype with
        | Heading1 -> rblock.mData |> h1BlockHandle
        | Para     -> rblock.mData |> paragraphBlockHandler
        | _        -> []

    match rawBlocks with
    | Error x -> []
    | Ok (rbList,linkRefList) ->
        rbList |> List.map callBlockHandler


[<EntryPoint>]
let main argv =

    Some ["lol"] 
    |> blockParser 
    |> blockDispatcher
    |> printfn "%A"
    0 // return an integer exit code

    ///// Block record type
    //type RawBlock = {blocktype: BlockId ; mData: string}


    //type BlockId =
        //| Heading6  | Heading5 | Heading4
        //| Heading3  | Heading2 | Heading1
        //| SetexHeading1
        //| SetexHeading2
        //| ThematicBreak
        //| Para
        //| BlockQuote 
        //| CBlock
        //| BlankLine
        //| LRefDec
        //| LRefDecB   // Beginning of link reference
        //| LRefD of LinkRefD
        //| List
        //| TableBlock
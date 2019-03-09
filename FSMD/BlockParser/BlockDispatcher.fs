module BlockDispatcher

open Types
open BlockHandlers

let rec blockDispatcher rawBlocks =
    let callBlockHandler rblock =
        match rblock.blocktype with
        | Para       -> rblock.mData |> paragraphBlockHandler
        | Heading1   -> rblock.mData |> h1BlockHandle
        | Heading2   -> rblock.mData |> h2BlockHandle
        | Heading3   -> rblock.mData |> h3BlockHandle
        | Heading4   -> rblock.mData |> h4BlockHandle
        | Heading5   -> rblock.mData |> h5BlockHandle
        | Heading6   -> rblock.mData |> h6BlockHandle
        | CBlock     -> rblock.mData |> codeBlockHandler
        | BlockQuote -> rblock.mData |> blockQuoteHandler blockDispatcher
        | List       -> failwithf "What? Block dispatcher for List not implemented"
        | TableBlock -> failwithf "What? Block dispatcher for Table not implemented"
        | _          -> []

    match rawBlocks with
    | Error x -> []
    | Ok (rbList,linkRefList) ->
        rbList |> List.collect callBlockHandler

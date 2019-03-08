module BlockDispatcher

open BlockParserTypes
open BlockParser
open BlockParserTests

// Future integration needed with corresponding blockhandler
(*
let blockDispatcher (groupedBlocks: Result<(RawBlock list*RawBlock list),string>) = 
    ///calling blockHandlers written by other group members
    ///input is string, output is Block list
    let genericBlockHandler (inputRawBlk:RawBlock) : Block list =
        match inputRawBlk with
        | rB when rB.blocktype=List       -> listBlockHandler rB.mData
        | rB when rB.blocktype=Table      -> tableBlockHandler rB.mData
        | rB when rB.blocktype=Paragraph  -> paragraphBlockHandler rB.mData
        | rB when rB.blocktype=BlockQuote -> blockQuoteBlockHandler rB.mData
        | rB when rB.blocktype=CodeBlock  -> codeBlockBlockHandler rB.mData
        | rB when rB.blocktype=Heading1   -> heading1BlockHandler rB.mData
        | rB when rB.blocktype=Heading2   -> heading2BlockHandler rB.mData
        | rB when rB.blocktype=Heading3   -> heading3BlockHandler rB.mData
        | rB when rB.blocktype=Heading4   -> heading4BlockHandler rB.mData
        | rB when rB.blocktype=Heading5   -> heading5BlockHandler rB.mData
        | rB when rB.blocktype=Heading6   -> heading6BlockHandler rB.mData
    
    let makeBlock (rawBlockList: RawBlock list) =
        rawBlockList
        |> List.fold (fun stateBlk curBlock -> genericBlockHandler curBlock) []

    Result.map (fun x -> (fst x)) groupedBlocks
    |> Result.map makeBlock
*)
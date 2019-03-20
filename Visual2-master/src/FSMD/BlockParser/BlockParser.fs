module BlockParser

open Types
open System
open System.IO
open System.Text.RegularExpressions

/// do regex on a line of string, return a tuple of (1st match, the rest of the string)
/// only when it's at the beginning of the string 
let (|RegexPat|_|) pat txt =
    let m = Regex.Match(txt, pat)
    match m.Success with
    | true -> 
              match m.Value with
                 | capString when (capString + txt.Substring(capString.Length) = txt) ->  (m.Value, txt.Substring(m.Value.Length)) |> Some
                 | _ -> None
    | false -> None

///Strips away the first '[' and the last ']:' from a link reference definition, returns a string of link ref content
let (|LRefTextId|_|) aLine =
    match aLine with
    | RegexPat "\s{0,3}\[.+\]:" line 
        -> match Seq.toList (fst line) with
           | '[' :: lcont -> let tempStr = lcont |> Array.ofList |> String
                             let m = Regex.Split(tempStr,"\]:")
                             Some (m.[0],snd line)
            | _  -> None
    | _ -> None


///Check whether the URL is valid or not
// REVISIT: Need to find out what constitutes a URL for REGEX
let (|LRefURLId|LRefURLInv|) str =
    match str with
    | RegexPat "\s*[a-zA-Z0-9./]+" lURL -> LRefURLId lURL
    | _                                 -> LRefURLInv

///Check whethere a title is valid, there is no title, or title is invalid
let (|LRefTitleId|LRefNoTitle|LRefInvalidT|) str =
    match str with
    | RegexPat "(\s*\".+\"\s*|\s*'.+'\s*)" ltitle 
        -> match snd ltitle with
           | RegexPat "[^\s*]" rest -> LRefInvalidT
           | _                      -> LRefTitleId ltitle
//  | RegexPat "[^\s*]" ltitle                 -> LRefInvalid
    | RegexPat "^\s*$" ltitle                  -> LRefNoTitle
    | _                                        -> LRefInvalidT


///Check whether a link reference block is valid or not, outputs different error message to be used
let lRefHandler lblock =
    match lblock with
    | {blocktype=bt ; mData=mD} 
      when bt=LRefDec -> 
        match mD with
        | LRefTextId lTxt' -> 
            match (snd lTxt') with
            | LRefURLId lURL' -> let charToTrim=[|' '; '\n';''';char(34)|]
                                 match (snd lURL') with
                                 | LRefTitleId lTl' -> Ok (LRefD {lText=(fst lTxt').Trim(charToTrim);
                                                                       lURL=(fst lURL').Trim(charToTrim);
                                                                       lTitle=Some((fst lTl').Trim(charToTrim))})
                                 | LRefNoTitle      -> Ok (LRefD {lText=(fst lTxt').Trim(charToTrim);
                                                                       lURL=(fst lURL').Trim(charToTrim);
                                                                       lTitle=None})
                                 | LRefInvalidT      -> Error <|sprintf "Invalid title"
            | LRefURLInv      -> Error <|sprintf "invalid URL"
        | _                -> Error <|sprintf "invalid text"
    | _                 -> Error <|sprintf "not lref block"

/// take in a line of string, do regex, and outputs a tuple of (BlockId*string)
let (|BlockIdentifier|) str :(BlockId*string) =
    match str with
    | RegexPat "#{6}#+\s*"  line -> (Para, str)
    | RegexPat "#{6}\s"     line -> (Heading6, snd line)
    | RegexPat "#{5}\s"     line -> (Heading5, snd line)
    | RegexPat "#{4}\s"     line -> (Heading4, snd line)
    | RegexPat "#{3}\s"     line -> (Heading3, snd line)
    | RegexPat "#{2}\s"     line -> (Heading2, snd line)
    | RegexPat "#\s"        line -> (Heading1, snd line)
    | RegexPat ">\s*"       line -> (BlockQuote, snd line)
    | RegexPat "(\s){0,3}(\*\s*\*\s*\*\s*|_\s*_\s*_\s*)+" line -> (ThematicBreak, str)
    | RegexPat "\s{0,3}(-)+" line when snd line = "" -> (SetexHeading2, "")
    | RegexPat "\s{0,3}(=)+" line when snd line = "" -> (SetexHeading1, "")
    | ""                                             -> (BlankLine,"") // No string, return blankline
    | RegexPat "\[.+\]:"            line             -> (LRefDecB, str)
    | RegexPat "\s{0,3}[0-9]+.\s+"  line             -> (List, str) // The number of whitespace doesn't matter
    | RegexPat "\s{0,3}[0-9]+\)\s+" line             -> (List, str)
    | RegexPat "\s{0,3}-\s+"        line             -> (List, str)
    | RegexPat "\s{0,3}\+\s+"       line             -> (List, str) // The number of whitespace doesn't matter
    | RegexPat "\s{0,3}\*\s+"       line             -> (List, str)
    | RegexPat "`{3}"               line             -> (CBlockT, snd line) // 3 backticks is also a codeblock
    | RegexPat "(\|\s---|\| )"      line             -> (TableBlock, str) // The number of whitespace doesn't matter
    | RegexPat @"\\"                line             -> match (snd line) with
                                                        | ""   -> (Para, "\\")
                                                        | rest -> (Para, rest) // \ is escape character, the rest parsed as paragraph
    | _                                              -> (Para, str) // Else, parse the entire line as paragraph

/// Parse line by line, convert each line into a tuple of (Block*string) and then group them accordingly

let blockParser stringList = 
    /// Function to convert each line into a tuple of (Block*string) and put in a list
    match stringList with
    | Some sList -> /// Accummulate a list of RawBlock 
                    let parseLine blockList line =
                         match line with
                         | BlockIdentifier parsedTup -> blockList @ [parsedTup] 

                    /// Group the blocks, reducing the (BlockId * string) list to be more compact
                    let groupBlocks ungroupBlocksLst =
                         let rec groupBlocks' ungroupLst groupedLst =
                             match ungroupLst, groupedLst with
                             //Initialisation
                             | hu::tu   , [] 
                                -> match fst hu with
                                   | LRefDecB      -> groupBlocks' tu [{blocktype=LRefDec;mData=snd hu}]
                                   | CBlockT -> groupBlocks' tu [{blocktype=CBlock;mData=snd hu}]
                                   | _ -> groupBlocks' tu [{blocktype= fst hu ; mData=snd hu}]
                             | hu::tu , hg::tg 
                                -> match fst hu , hg.blocktype with
                                   //If setexheading after a paragraph, change the previous paragraph into heading
                                   | SetexHeading1, Para -> groupBlocks' tu ([{blocktype= Heading1; mData=hg.mData}] @ tg) 
                                   | SetexHeading2, Para -> groupBlocks' tu ([{blocktype= Heading2; mData=hg.mData}] @ tg)
                                   //If setextheading after not a paragraph, a new paragraph block is formed
                                   | SetexHeading1, b when (not (b=Para)) -> let bNew = {blocktype=Para; mData= snd hu}
                                                                             groupBlocks' tu ([bNew] @ (hg::tg))
                                   | SetexHeading2, b when (not (b=Para)) -> let bNew = {blocktype=Para; mData= snd hu}
                                                                             groupBlocks' tu ([bNew] @ (hg::tg))
                                   //A new link reference declaration is captured, make a new LRefDec block
                                   | LRefDecB, _      -> let bNew = {blocktype=LRefDec; mData=snd hu}
                                                         groupBlocks' tu ([bNew] @ (hg::tg))
                                   //If current block is a list, if the next one is not blankline
                                   //A new codeblock tag is captured, make a CBlock
                                   | CBlockT, a when not(a=CBlock)  -> let bNew = {blocktype=CBlock; mData=""}
                                                                       groupBlocks' tu ([bNew] @ (hg::tg))
                                   //If a CBlockT is found after CBlock, it terminates CBlock (add blankline)
                                   | CBlockT, CBlock  ->
                                     let bNew = {blocktype=BlankLine; mData=""}
                                     groupBlocks' tu ([bNew] @ (hg::tg))
                                   //Lazy continuation of container blocks,append \n to current block mData
                                   | Para, a when a=List
                                                       ||a=BlockQuote||a=LRefDec||a=CBlock    -> 
                                     groupBlocks' tu ([{blocktype=a; mData=hg.mData+"\n"+snd hu}] @ tg)
                                   //If current block is the same as previous block, append to previous block mData
                                   | a , b when a = b 
                                                && (not (a=BlankLine)) && (not(a=CBlock)) 
                                                && (not (a=LRefDec)) -> 
                                     groupBlocks' tu ([{blocktype= b; mData=hg.mData+"\n"+snd hu}] @ tg)
                                   //Else,create a new block
                                   | a , b            -> let bNew = {blocktype=a; mData=snd hu}
                                                         groupBlocks' tu ([bNew] @ (hg::tg))
                             //If ungroupLst is empty, parsing finished, return groupedLst
                             | []     , _                                -> groupedLst
                       
                         groupBlocks' ungroupBlocksLst []
                         |> List.rev // reverse the list
                     
                    ///Check linkrefdec block, if valid, output LREF block, else a paragraph
                    let checkLinkRefDecBlock (block: RawBlock) =
                        match lRefHandler block with
                        | Ok bRes                -> {blocktype=bRes; mData=""}
                        | Error "Invalid title"  -> {blocktype=Para; mData=block.mData} //Invalid title,becomes a paragraph block
                        | Error "not lref block" -> {blocktype=block.blocktype; mData=block.mData} //Not LRefD, passes through
                        | Error e                -> {blocktype=Para; mData=block.mData} //Other parsing error, becomes a paragraph

                    ///Accummulate (BlockId*string) into a list for grouping
                    let blkIdStringTupLst = List.fold (fun stateBlockLst curLineStr -> parseLine stateBlockLst curLineStr) [] sList
                    
                    ///Group the (BlockId*string)list into RawBlock list
                    ///Also convert FencedCBlock and SpaceCBlock into CBlock
                    ///And check lRefBlock
                    let groupedwithLinkRef =
                        groupBlocks blkIdStringTupLst
                        |> List.filter (fun blk -> not (blk.blocktype = BlankLine)) //remove blanklines
                        |> List.fold (fun stateBlockLst curBlock -> stateBlockLst @[checkLinkRefDecBlock curBlock]) [] //check LRef

                    ///Put valid linkreference in a list for further referencing use
                    let LRefList =
                        groupedwithLinkRef
                        |> List.filter (fun blk -> match blk.blocktype with
                                                    | LRefD l -> true
                                                    | _       -> false)
                        |> List.fold (fun lReflst lRefBlk ->    match lRefBlk with
                                                                |{blocktype=LRefD l; mData= m} -> (lReflst @ [l])
                                                                | _                           -> []) []

     
                    ///Groupedblocks to be sent to the block dispacther
                    let groupedReadyBlocks =
                        groupedwithLinkRef
                        |> List.filter (fun blk -> match blk.blocktype with
                                                    | LRefD a -> false
                                                    | _       -> true)
                    //Output a tuple of RawBlock list * LinkRefD list
                    (groupedReadyBlocks,LRefList) |> Ok

    | Some sList when sList.IsEmpty -> Error <| sprintf "Parsing failed, initial input string list is empty"
    | None -> Error <| sprintf "Parsing failed, no input list is given"
    
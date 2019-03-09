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
      when bt=LRefDec  -> 
      match mD with
      | LRefTextId lTxt' -> match (snd lTxt') with
                            | LRefURLId lURL' -> let charToTrim=[|' '; '\n';''';'\"'|]
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
    | RegexPat "\s{4}"      line -> (CBlock, snd line) // 4 whitespace is a codeblock
    | RegexPat "(\s){0,3}(\*\s*\*\s*\*\s*|_\s*_\s*_\s*)+" line -> (ThematicBreak, str)
    | RegexPat "\s{0,3}(-)+" line when snd line = "" -> (SetexHeading2, "")
    | RegexPat "\s{0,3}(=)+" line when snd line = "" -> (SetexHeading1, "")
    | ""                                             -> (BlankLine,"") // no string, return blankline
    | RegexPat "\[.+\]:"   line                      -> (LRefDecB, str)
    | RegexPat "\s{0,3}[0-9]+.\s+"  line             -> (List, str) // the number of whitespace doesn't matter
    | RegexPat "\s{0,3}[0-9]+\)\s+" line             -> (List, str)
    | RegexPat "\s{0,3}-\s+" line                    -> (List, str)
    | RegexPat "\s{0,3}\+\s+" line                   -> (List,str) // the number of whitespace doesn't matter
    | RegexPat "\s{0,3}\*\s+" line                   -> (List,str)
    | RegexPat "(\|\s---|\| )" line                  -> (TableBlock,str) // the number of whitespace doesn't matter
    | RegexPat @"\\"          line                   -> match (snd line) with
                                                        | ""   -> (Para, "\\")
                                                        | rest -> (Para, rest) // \ is escape character, the rest parsed as paragraph
    | _                                              -> (Para, str) // else, parse the entire line as paragraph

/// parse line by line, convert each line into a tuple of (Block*string) and then group them accordingly

let blockParser stringList = 
    /// function to convert each line into a tuple of (Block*string) and put in a list
    match stringList with
    | Some sList -> /// accummulate a list of RawBlock 
                    let parseLine blockList line =
                         match line with
                         | BlockIdentifier parsedTup -> blockList @ [parsedTup] 

                    /// group the blocks, reducing the (BlockId * string) list to be more compact
                    let groupBlocks ungroupBlocksLst =
                         let rec groupBlocks' ungroupLst groupedLst =
                             match ungroupLst, groupedLst with
                             //initialisation
                             | hu::tu   , [] 
                                -> match fst hu with
                                   | LRefDecB -> groupBlocks' tu [{blocktype=LRefDec;mData=snd hu}]
                                   | _ -> groupBlocks' tu [{blocktype= fst hu ; mData=snd hu}]
                             | hu::tu , hg::tg 
                                -> match fst hu , hg.blocktype with
                                   //if setexheading after a paragraph, change the previous paragraph into heading
                                   | SetexHeading1, Para -> groupBlocks' tu ([{blocktype= Heading1; mData=hg.mData}] @ tg) 
                                   | SetexHeading2, Para -> groupBlocks' tu ([{blocktype= Heading2; mData=hg.mData}] @ tg)
                                   //if setextheading after not a paragraph, a new paragraph block is formed
                                   | SetexHeading1, b when (not (b=Para)) -> let bNew = {blocktype=Para; mData= snd hu}
                                                                             groupBlocks' tu ([bNew] @ (hg::tg))
                                   | SetexHeading2, b when (not (b=Para)) -> let bNew = {blocktype=Para; mData= snd hu}
                                                                             groupBlocks' tu ([bNew] @ (hg::tg))
                                   //a new link reference declaration is captured, make a new LRefDec block
                                   | LRefDecB, _ -> let bNew = {blocktype=LRefDec; mData=snd hu}
                                                    groupBlocks' tu  ([bNew] @ (hg::tg))
                                   //Lazy continuation of container blocks,append \n to current block mData
                                   | Para, a when a=List
                                                       ||a=BlockQuote||a=LRefDec -> 
                                                       groupBlocks' tu  ([{blocktype=a; mData=hg.mData+"\n"+snd hu}] @ tg)
                                   //If current block is the same as previous block, append to previous block mData
                                   | a , b when a = b 
                                                && (not (a=BlankLine)) 
                                                && (not (a=LRefDec)) -> 
                                                       groupBlocks' tu ([{blocktype= b; mData=hg.mData+"\n"+snd hu}] @ tg)
                                   //else,create a new block
                                   | a , b            -> let bNew = {blocktype=a; mData=snd hu}
                                                         groupBlocks' tu ([bNew] @ (hg::tg))
                             // if ungroupLst is empty, parsing finished, return groupedLst
                             | []     , _                                -> groupedLst
                       
                         groupBlocks' ungroupBlocksLst [] |> List.rev // reverse the list
                     
                    ///check linkrefdec block, if valid, output LREF block, else a paragraph
                    let checkLinkRefDecBlock (block: RawBlock) =
                        match lRefHandler block with
                        | Ok bRes                -> {blocktype=bRes; mData=""}
                        | Error "Invalid title"  -> {blocktype=Para; mData=block.mData} //invalid title,becomes a paragraph block
                        | Error "not lref block" -> {blocktype=block.blocktype; mData=block.mData} //not LRefD, passes through
                        | Error e                -> {blocktype=Para; mData=block.mData} //other parsing error, becomes a paragraph

                    ///accummulate (BlockId*string) into a list for grouping
                    let blkIdStringTupLst = List.fold (fun stateBlockLst curLineStr -> parseLine stateBlockLst curLineStr) [] sList
                    
                    ///group the (BlockId*string)list into RawBlock list
                    ///and check lRefBlock
                    let groupedwithLinkRef =
                        groupBlocks blkIdStringTupLst
                        |> List.filter (fun blk -> not (blk.blocktype = BlankLine)) //remove blanklines
                        |> List.fold (fun stateBlockLst curBlock -> stateBlockLst @[checkLinkRefDecBlock curBlock] ) [] //check LRef

                    ///put valid linkreference in a list for further referencing use
                    let LRefList =
                        groupedwithLinkRef
                        |> List.filter (fun blk -> match blk.blocktype with
                                                    | LRefD l -> true
                                                    | _      -> false)
                        |> List.fold (fun lReflst lRefBlk ->    match lRefBlk with
                                                                |{blocktype=LRefD l; mData= m} -> (lReflst @ [l])
                                                                | _                           -> []) []

     
                    ///groupedblocks to be sent to the block dispacther
                    let groupedReadyBlocks =
                        groupedwithLinkRef
                        |> List.filter (fun blk -> match blk.blocktype with
                                                    | LRefD a -> false
                                                    | _       -> true)
                    //output a tuple of RawBlock list * LinkRefD list
                    (groupedReadyBlocks,LRefList) |> Ok

    | Some sList when sList.IsEmpty -> Error <| sprintf "Parsing failed, initial input string list is empty"
    | None -> Error <| sprintf "Parsing failed, no input list is given"

//===============================================================================================================
//                                    Main Execution of blockParser
//===============================================================================================================
 
/// given a path with markdown file inside, read line by line and store in an array
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

/// RawBlock list to be sent to Block Dispatcher          
let a =
    match (blockParser linesList) with
        | Ok res  -> (fst res)
        | Error e -> []
/// LinkRefD list to be sent to Block Dispatcher          
let b =
    match (blockParser linesList) with
        | Ok res  -> (snd res)
        | Error e -> []
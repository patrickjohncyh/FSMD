open System
open System.IO
open System.Security.Policy
open System.Text.RegularExpressions

type Token = 
    | Exclamation // !  
    | QuoteMark   // "   
    | Hash        // #  
    | Dollar      // $   
    | Percent     // %  
    | Ampersand   // &   
    | Apostrophe  // '
    | RBracketO   // (
    | RBracketC   // )
    | Asterisk    // *
    | Plus        // +
    | Comma       // ,  
    | Minus       // -
    | Period      // .  
    | Backslash   // /  
    | Colon       // : 
    | Semicolon   // ;  
    | LessThan    // <   
    | Equals      // =  
    | MoreThan    // >  
    | QuesMark   // ?
    | At          // @
    | SBracketO   // [
    | Forwardslash// \
    | SBracketC   // ]
    | Caret       // ^
    | Underscore  // _  
    | Backtick    // `  
    | CBracketO   // {
    | VerticalBar // |
    | CBracketC   // }
    | Tilde       // ~
    | Whitespace  //  
    | Text of string

type BlockId =
    | Heading6 
    | Heading5
    | Heading4
    | Heading3
    | Heading2
    | Heading1
    | SetexHeading1
    | SetexHeading2
    | Paragraph
    | BlockQuote 
    | OrderedList
    | UnorderedList
    | CodeBlock
    | ThematicBreak
    | BlankLine
    | LinkReferenceBegin
    | LinkReferenceEnd
    | LinkReference
/// Block record type
type Block = {blocktype: BlockId ; mData: string list}
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

/// take in a line of string, do regex, and outputs a tuple of (BlockId*string)
let (|BlockIdentifier|) (str: string) =
    match str with
    | RegexPat "#{6}#+\s*"  line -> (Paragraph, str)
    | RegexPat "#{6}\s"     line -> (Heading6, snd line)
    | RegexPat "#{5}\s"     line -> (Heading5, snd line)
    | RegexPat "#{4}\s"     line -> (Heading4, snd line)
    | RegexPat "#{3}\s"     line -> (Heading3, snd line)
    | RegexPat "#{2}\s"     line -> (Heading2, snd line)
    | RegexPat "#\s"        line -> (Heading1, snd line)
    | RegexPat ">\s*"         line -> (BlockQuote, snd line)
    | RegexPat "\s\s\s\s"      line -> (CodeBlock, snd line) // 4 whitespace is a codeblock
    | RegexPat "(\s){0,3}(\*\s*\*\s*\*\s*|_\s*_\s*_\s*)+" line -> (ThematicBreak, snd line) // REVISIT thematic break syntax - is used in setex heading too.
    | RegexPat "\s{0,3}(-)+" line when snd line = "" -> (SetexHeading2, "")
    | RegexPat "\s{0,3}(=)+" line when snd line = "" -> (SetexHeading1, "")
    | ""                        -> (BlankLine,"") // no string, return blankline
    | RegexPat "\[.+\]:"   line  -> (LinkReferenceBegin, snd line)
    | RegexPat "\[.+\]"    line -> (LinkReferenceEnd, snd line)
    | RegexPat "[0-9]+(\)|.)\s"  line -> (OrderedList, snd line)
    | RegexPat "(-|\*)\s" line -> (UnorderedList, snd line)
    | _                         -> (Paragraph, str) // else, parse the entire line as paragraph

/// given a path with markdown file inside, read line by line and store in an array
let fileStream (fPath:string) =
    try
        File.ReadAllLines(fPath) |> Ok
    with
    | :? System.ArgumentException           -> Error <| sprintf "Path is a zero-length string"
    | :? System.UnauthorizedAccessException -> Error <| sprintf "Non authorized access to file"
    | :? System.IO.FileNotFoundException    -> Error <| sprintf "Could not find the file %A" fPath

/// convert string array into list array
let lineList = fileStream @"markdown.txt" 
                |> function | Ok sList -> Some (Array.toList sList) | Error _ -> None

/// parse line by line, convert each line into a tuple of (Block*string) and then group them accordingly
let parseBlocks (stringList : string list option ) = 
    /// function to convert each line into a tuple of (Block*string) and put in a list
    match stringList with
    | Some sList ->  
                    let parseLine (blockList : (BlockId*string) list) (line : string) =
                         match line with
                         | BlockIdentifier parsedTup -> blockList @ [parsedTup] 
                     /// function to combine two consecutive blocks if they are of the same DU (except BlankLine)
                    let appendTwoBlocks (b1:Block) (b2:Block) =
                        {blocktype = b1.blocktype ;  mData = b1.mData @ b2.mData}
                     /// group the blocks, reducing the (BlockId * string) list to be more compact
                    let groupBlocks (ungroupBlocksLst : (BlockId * string) list) : Block list =
                         let rec groupBlocks' (ungroupLst : (BlockId * string) list)  (groupedLst :Block list) =
                             match ungroupLst, groupedLst with
                             // if groupLst empty, initialise with the 1st block from ungrouped part
                             | hu::tu   , []                             -> groupBlocks' tu [{blocktype= fst hu ; mData=[snd hu]}]
                             // if current line is "=" or "-" and previous one is a paragraph, change it into heading (setex heading)
                             | hu::tu , hg::tg when (fst hu = SetexHeading1) && (hg.blocktype = Paragraph) -> groupBlocks' tu ([{blocktype= Heading1; mData=hg.mData}] @ tg)
                             | hu::tu , hg::tg when (fst hu = SetexHeading2) && (hg.blocktype = Paragraph) -> groupBlocks' tu ([{blocktype= Heading2; mData=hg.mData}] @ tg)
                             // if head of groupLst = head of ugList, append the string to gList, except blankline
                             | hu::tu , hg::tg when (fst hu) = (hg.blocktype) && not (fst hu = BlankLine)  -> let bu = {blocktype=fst hu; mData=[snd hu]}
                                                                                                              groupBlocks' tu ([appendTwoBlocks hg bu] @ tg)
                             // if the current block not the same as previous, new block group is formed
                             | hu::tu , hg::tg                            -> let bNew = {blocktype=fst hu; mData=[snd hu]}
                                                                             groupBlocks' tu ([bNew] @ (hg::tg)) 
                             // if ungroupLst is empty, parsing finished, return groupedLst
                             | []     , _                                -> groupedLst
                         groupBlocks' ungroupBlocksLst [] |> List.rev
                    let ungroupedBlockLst = List.fold (fun stateBlockLst curLineStr -> parseLine stateBlockLst curLineStr) [] sList 
                    groupBlocks ungroupedBlockLst |> List.filter (fun blk -> not (blk.blocktype = BlankLine)) |> Ok // remove blanklines
    | Some sList when sList.IsEmpty -> Error <| sprintf "Parsing failed, initial input string list is empty"
    | None -> Error <| sprintf "Parsing failed, no input list is given" 
parseBlocks lineList
(*
let parseBlocks (stringList : string list option ) = 
    /// function to convert each line into a tuple of (Block*string) and put in a list
    match stringList with
    | Some sList ->  
                     let parseLine (blockList : (BlockId*string) list) (line : string) =
                         match line with
                         | BlockIdentifier parsedTup -> blockList @ [parsedTup]
    
                     /// function to combine two consecutive blocks if they are of the same DU (except BlankLine)
                     let appendTwoBlocks (b1:(BlockId * string)) (b2: (BlockId * string)) =
                         let bNew = (snd b1) + "\n" + (snd b2)
                         (fst b1),bNew
                     /// group the blocks, reducing the (Block * string) list to be more compact
                     let groupBlocks ungroupBlocksLst : (BlockId * string) list =
                         let rec groupBlocks' (ungroupLst : (BlockId * string) list)  (groupedLst :(BlockId * string) list) =
                             match ungroupLst, groupedLst with
                             // if groupLst empty, initialise with the 1st block from ungrouped part
                             | hu::tu   , []                             -> groupBlocks' (tu) ([hu])
                             // if current line is "=" or "-" and previous one is a paragraph, change it into heading (setex heading)
                             | hu::tu , hg::tg when (snd hu = "=") && (fst hg = Paragraph) -> groupBlocks' (tu) ([Heading1,(snd hg)] @ tg) 
                             // if head of groupLst = head of ugList, append the string to gList, except blankline
                             | hu::tu , hg::tg when (fst hu) = (fst hg) && not (fst hu = BlankLine)  -> groupBlocks' (tu) ([appendTwoBlocks hg hu] @ tg)
                             // if the current block not the same as previous, new block group is formed
                             | hu::tu , hg::tg                            -> groupBlocks' (tu) ([hu] @ (hg::tg)) 
                             // if ungroupLst is empty, parsing finished, return groupedLst
                             | []     , _                                -> groupedLst

                         groupBlocks' ungroupBlocksLst [] |> List.rev
 
                     let ungroupedBlockLst = List.fold (fun stateBlockLst curLineStr -> parseLine stateBlockLst curLineStr) [] sList 
                     // remove blanklines

                     groupBlocks ungroupedBlockLst |> List.filter (fun blk -> not (fst blk = BlankLine)) |> Ok
    | Some sList when sList.IsEmpty -> Error <| sprintf "Parsing failed, initial input string list is empty"
    | None -> Error <| sprintf "Parsing failed, no input list is given" *)
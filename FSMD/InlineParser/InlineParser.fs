module InlineParser

open Types
open InlineParserHelpers

let rec inlineTokeniser txt = 
    let escapedPat = "\\\[!-/:-@\[-`{-~]"
    match txt with
    | "" -> []
    | _  ->
        let (newToken,remain) = 
            match txt with
            | RegexPrefix2 escapedPat (str,remain) -> ((Escaped str.[1..]),remain)
            | RegexPrefix "\\n"    remain -> (Newline,remain)
            | RegexPrefix "[\s]"   remain -> (Whitespace,remain)
            | RegexPrefix "[\\\]"  remain -> (Backslash,remain)
            | RegexPrefix "\*\*\(" remain -> (StrongOpenAst,remain)
            | RegexPrefix "\)\*\*" remain -> (StrongCloseAst,remain)
            | RegexPrefix "\*\("   remain -> (EmpOpenAst,remain)
            | RegexPrefix "\)\*"   remain -> (EmpCloseAst,remain)
            | RegexPrefix "__\("   remain -> (StrongOpenUnd,remain)
            | RegexPrefix "\)__"   remain -> (StrongCloseUnd,remain)
            | RegexPrefix "_\("    remain -> (EmpOpenUnd,remain)
            | RegexPrefix "\)_"    remain -> (EmpCloseUnd,remain)
            | RegexPrefix "\<"     remain -> (LessThan,remain)
            | RegexPrefix "\>"     remain -> (MoreThan,remain)
            | RegexPrefix "\["     remain -> (SBracketO,remain)
            | RegexPrefix "\]"     remain -> (SBracketC,remain)
            | RegexPrefix "\("     remain -> (RBracketO,remain)
            | RegexPrefix "\)"     remain -> (RBracketC,remain)
            | RegexPrefix "_"      remain -> (Underscore,remain)
            | RegexPrefix "\*"     remain -> (Asterisk,remain)
            | RegexPrefix "\!"     remain -> (Exclamation,remain)
            | RegexPrefix "[\"]"   remain -> (QuoteMark,remain)
            | RegexPrefix2 "[`]+"         (str,remain) -> (BacktickStr str.Length,remain)
            | RegexPrefix2 "[A-Za-z0-9]+" (str,remain) -> ((Literal str),remain)
            | RegexPrefix2 "."            (str,remain) -> ((Literal str),remain)
            | _ -> failwithf "What? Should not happen as . Regex caputers all"

        match (inlineTokeniser remain) with
        | [] -> [newToken]
        | tokenList -> newToken :: tokenList


let rec parser tokens = 
    // Converts reamaining tokens into Styled Text
    // Combine consecutive Text into a single Styled Text
    let parseText tokens =
        let combineAndConvertToText state token =
            let (accStr,out) = state
            match accStr,token with
            | "",Styled a   -> ("",Styled a::out)
            | s ,Styled a   -> ("",Styled a::Styled (Text s)::out)
            | s ,notStyled  -> 
                let strToken = notStyled |> token2String 
                (s+strToken,out)

        (("",[]),tokens) 
        ||> List.fold combineAndConvertToText 
        |>  function
            | "",out -> out
            | s ,out -> Styled(Text s)::out // Convert any remaining string
        |> List.rev


    // Convert tokens to Text Inline Elements
    let toText tokens = 
        tokens |> parseText |> styledToInlineElement


    let rec parseCodeSpan tokens =
        let openDelim = tokens |> List.tryFind (function | BacktickStr a -> true | _ -> false)
        match openDelim with 
        | Some delim ->
            match tokens with
            | TryParseWith delim delim (before,inner,after)
                -> before @ [inner|>toText|>CodeSpan|>Styled] @ (parseCodeSpan after)
            | _ -> tokens |> parseRest delim parseCodeSpan
        | None -> tokens


    // Genenral Image or Link parser
    let rec parseLinkOrImg config tokens =
        // Build specific parser for recursive call  
        let thisParser = parseLinkOrImg config  
        let ( (|ParseText|_|) , typeConst) = config

        match tokens with
            | ParseText (before,text,ParseDest (dest,after))
                ->  let linkInfo = {linkText=text |> parser |> styledToInlineElement;
                                    linkDest=dest |> toText;
                                    linkTitle=None}
                    match after with 
                    | ParseTitle (title,after2) // Link/Image with title 
                        -> let linkTitle = title |> toText |> Some
                           before @ [{linkInfo with linkTitle=linkTitle}|>typeConst|>Styled] @ (thisParser after2)
                    | _                        // Link/Image without title      
                        -> before @ [linkInfo|>typeConst|>Styled] @ (thisParser after)
            | _ -> tokens |> parseRest SBracketO thisParser 


    //General Image Reference or Link Reference parser
    let rec parseLinkOrImgRef config tokens =
        // Build specific parser for recursive call
        let thisParser = parseLinkOrImgRef config 
        let ( (|ParseText|_|) , typeConst) = config
        match tokens with 
        | ParseText (before,(text:Token list),after)  when (not text.IsEmpty)
            -> let textStr = text |> tokenList2String
               let refInfo = {linkText=textStr;linkRef = None}
               match after with
               | ParseRef (refr,after2) 
                   -> before  @ [{refInfo with linkRef=refr}|>typeConst|>Styled] @ (thisParser after2)
               | _ -> before  @ [refInfo|>typeConst|>Styled] @ (thisParser after)
        | _ -> tokens |> parseRest SBracketO thisParser 


    let rec parseBreaks tokens = 
        // Split up tokens
        // strip whitespace
        // reverse before to original
        let makeOutput pType before n tokens=
            tokens |> List.splitAt (n+1)
                   |> snd
                   |> fun after -> (before |> stripWSHead |>List.rev) @ [pType|>Styled]  @ (after |> stripWSHead |> parseBreaks)
                   
        // Check for Softbreak and Hardbreak conditions if Newline exists
        match tokens with
        | FindTokenIdx Newline n -> 
                match List.rev tokens.[..n-1] with 
                | []  -> []
                | Backslash::before               // Harbreak, Backslash                     
                    ->  makeOutput Hardbreak before n tokens 
                | Whitespace::Whitespace::before  // Hardbreak, >=2 Whitespace                  
                    ->  makeOutput Hardbreak before n tokens
                | before                          // Softbrak, <2 Whitespace
                    ->  makeOutput Softbreak before n tokens 
        | _   -> tokens       


    let parseEmOrStrong config tokens  =
        let (delims,typeConst) = config

        let rec genericParser typeConst openDelim closeDelim tokens = 
            let thisParser = genericParser typeConst openDelim closeDelim 
            match tokens with
            | TryParseWith openDelim closeDelim (before,inner,after)
                ->  before 
                    @ [inner|> parser |> styledToInlineElement |>typeConst|>Styled]
                    @ (thisParser after)
            | _
                -> tokens |> parseRest openDelim thisParser

        // Build parsers based on delimiters and type constructor 
        let parsers = delims 
                    |> List.map ( fun (oDelim,cDelim) -> (oDelim,cDelim) ||> genericParser typeConst )
        // Apply parsers sequentially
        (tokens,parsers) ||> List.fold (fun toks psr -> psr toks) 



    // Build Image and Link parsers based on config        
    let parseImage = parseLinkOrImg imageConfig
    let parseLink  = parseLinkOrImg linkConfig

    // Build ImageRef and Link Ref parsers based on config
    let parseImageRef = parseLinkOrImgRef imageRefConfig
    let parseLinkRef  = parseLinkOrImgRef linkRefConfig

    // Build Strong and Em parsers based on config
    let parseStrong   =  parseEmOrStrong strongConfig
    let parseEmphasis =  parseEmOrStrong empConfig 


    // Main Parsing execution
    tokens 
    |> parseCodeSpan
    |> parseImage
    |> parseLink
    |> parseImageRef
    |> parseLinkRef
    |> parseStrong
    |> parseEmphasis
    |> parseBreaks
    |> parseText

   
// Top level function
let inlineParser inputString =
    inputString 
    |> inlineTokeniser 
    |> parser 
    |> styledToInlineElement
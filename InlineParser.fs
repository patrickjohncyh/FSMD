module InlineParser

open InlineTypes
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
        let openDelimIdx = tokens |> List.tryFindIndex (function | BacktickStr a -> true | _ -> false)
        match openDelimIdx with 
        | Some idx ->
            let openDelim = tokens.[idx]
            match tokens with
            | TryParseWith openDelim openDelim (before,inner,after)
                -> before @ [inner|>toText|>CodeSpan|>Styled] @ (parseCodeSpan after)
            | _ 
                -> let (before,after) = tokens |> List.splitAt (idx+1)
                   before @ (parseCodeSpan after)
        | None -> tokens



    let rec parseLinksOrImg tokens =
        let makeLinkInfo text dest =  {linkText=text |> parser |> styledToInlineElement;
                                       linkDest=dest |> toText;
                                       linkTitle=None}
        let basicParse =
            match tokens with
            // Try to Parse text and dest of Image or Link
            | ParseTextImg (before,text,ParseDest (dest,after))
                -> Some (before,makeLinkInfo text dest,after,Image)
            | ParseTextLink (before,text,ParseDest (dest,after))
                -> Some (before,makeLinkInfo text dest,after,Link)
            | _ -> None 
       
        match basicParse with
        | Some (before,linkInfo,after,typeConst)
            -> match after with 
                | ParseTitle (title,after2) // Link with title 
                    -> let linkTitle = title |> toText |> Some
                       before @ [{linkInfo with linkTitle= linkTitle}|>typeConst|>Styled] @ (parseLinksOrImg after2)
                | _                        // Link without title      
                    -> before @ [linkInfo|>typeConst|>Styled] @ (parseLinksOrImg after)
        | None  -> match tokens with 
                   | FindTokenIdx SBracketO idx 
                        ->tokens 
                          |> List.splitAt (idx+1) 
                          |> (fun (before,after)->before @ (parseLinksOrImg after))
                   | _ -> tokens


    //image ref or link ref
    let rec parseLinkOrImgRef tokens =
        let basicParse = 
            match tokens with 
            | ParseTextImg (before,text,after)  when (not text.IsEmpty)
                -> (before,text,after,ImageRef) |> Some
            | ParseTextLink (before,text,after) when (not text.IsEmpty)
                -> (before,text,after,LinkRef)  |> Some
            | _ -> None

        match basicParse with
        | Some (before, text,after,typeConst) 
               -> let textStr = text |> tokenList2String
                  let refInfo = {linkText=textStr;linkRef = None}
                  match after with
                  | ParseRef (refr,after2) 
                      -> before  @ [{refInfo with linkRef=refr}|>typeConst|>Styled] @ (parseLinkOrImgRef after2)
                  | _ -> before  @ [refInfo|>typeConst|>Styled] @ (parseLinkOrImgRef after)
        | None -> match tokens with 
                   | FindTokenIdx SBracketO idx 
                        ->tokens 
                          |> List.splitAt (idx+1) 
                          |> (fun (before,after)->before @ (parseLinkOrImgRef after))
                   | _ -> tokens
                   


    let parseEmOrStrong typeConst delims tokens  =
        let rec genericParser typeConst openDelim closeDelim tokens = 
            let thisParser = genericParser typeConst openDelim closeDelim 
            match tokens with
            | TryParseWith openDelim closeDelim (before,inner,after)
                ->  before 
                    @ [parser inner|> styledToInlineElement |>typeConst|>Styled]
                    @ (thisParser after)
            | _
                -> match tokens with
                   | FindTokenIdx openDelim idx 
                        ->  tokens 
                            |> List.splitAt (idx+1)
                            |> (fun (before,after) -> before @ (thisParser after))
                   | _ -> tokens
        // Make parsers based on delimiters and type constructor 
        let parsers = delims 
                    |> List.map ( fun (oDelim,cDelim) -> (oDelim,cDelim) ||> genericParser typeConst )

        // Apply parsers sequentially
        (tokens,parsers) ||> List.fold (fun toks psr -> psr toks) 



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
        
    // Delimiters for Strong and Emphasis  
    let strongDelims = [(StrongOpenAst,StrongCloseAst);(StrongOpenUnd,StrongCloseUnd)]
    let empDelims   =  [(EmpOpenAst,EmpCloseAst);(EmpOpenUnd,EmpCloseUnd)]

    // Main Parsing execution
    tokens 
    |> parseCodeSpan
    |> parseLinksOrImg
    |> parseLinkOrImgRef
    |> parseEmOrStrong Strong strongDelims
    |> parseEmOrStrong Emphasis empDelims
    |> parseBreaks
    |> parseText

   
// Top level function
let inlineParser inputString =
    inputString 
    |> inlineTokeniser 
    |> parser 
    |> styledToInlineElement
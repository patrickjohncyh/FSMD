module InlineParser

open System.Text.RegularExpressions

type InlineElement = 
    | Link      of LinkInfo
    | Image     of LinkInfo
    | CodeSpan  of InlineElement list
    | Strong    of InlineElement list
    | Emphasis  of InlineElement list
    | Text      of string
    | Hardbreak 
    | Softbreak

and LinkInfo = {linkText  : InlineElement list;
                linkDest  : InlineElement list; 
                linkTitle : InlineElement list option} 
type Token = 
    | SBracketO     | SBracketC      | RBracketO     | RBracketC       | QuoteMark  | Whitespace    
    | Newline       | Exclamation    | Asterisk      | EmpOpenAst      | EmpCloseAst| EmpOpenUnd | EmpCloseUnd  
    | StrongOpenAst | StrongCloseAst | StrongOpenUnd | StrongCloseUnd  | Underscore | LessThan   | MoreThan   
    | Backslash
    | Escaped     of string
    | Literal     of string
    | BacktickStr of int
    | Styled      of InlineElement

// Converts a token to string representation
let (|Token2String|) token = 
    let translation = [ SBracketO,"["   ;       SBracketC,"]";      RBracketO,"(";      RBracketC,")";      QuoteMark,"\""; 
                        Whitespace," "  ;       Newline, "\\n";      Exclamation,"!";    Asterisk,"*";       EmpOpenAst,"*(";
                        EmpCloseAst,")*";       EmpOpenUnd,"_(";    EmpCloseUnd,")_";   Underscore,"_";     LessThan,"<";
                        MoreThan,">";           Backslash,"\\";
                        StrongOpenAst ,"**("
                        StrongCloseAst,")**"
                        StrongOpenUnd ,"__("
                        StrongCloseUnd,")__"] |> Map.ofList
    match token with
    | Literal s -> s
    | Escaped s -> s
    | BacktickStr n -> "`" |> String.replicate n
    | tok             -> 
        match (Map.tryFind tok translation)  with
        | Some s -> s
        | None   -> failwithf "What? Trying to convert token with no translation to string"

let (|RegexPrefix|_|) pat txt =
    let m = Regex.Match(txt,"^"+ pat)
    match m.Success with
    | true -> (txt.Substring(m.Value.Length)) |> Some
    | false -> None
    
let (|RegexPrefix2|_|) pat txt =
    let m = Regex.Match(txt,"^"+ pat)
    match m.Success with
    | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
    | false -> None


let escapedPat = "\\\[\\!-\\/\\:-\\@\\[-\\`\\{\\~]"

let rec inlineTokeniser txt = 
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

let rec stripWSHead tokens = 
            match tokens with
            | []      -> []
            | Whitespace::rlst ->  stripWSHead rlst
            | _ -> tokens

let rec stripWSTail tokens = 
           tokens 
           |> List.rev 
           |> stripWSHead 
           |> List.rev
           
let (|FindTokenIdx|_|) token tokens = 
    tokens |> List.tryFindIndex (fun x -> x = token)

let findTokenIdx = (|FindTokenIdx|_|)

let balancedCloseIdx openTok closeTok tokens startIdx =
    let folder state tok = 
        let (lvl,idx) = state
        let nIdx = idx+1  
        match lvl,tok with 
        | 0,_                 -> (lvl  ,idx)
        | 1,t when t=closeTok -> (lvl-1,idx)          
        | n,t when t=closeTok -> (lvl-1,nIdx)
        | n,t when t=openTok  -> (lvl+1,nIdx)
        | n,_                 -> (lvl  ,nIdx)
    tokens
    |> List.splitAt (startIdx+1)
    |> snd
    |> fun x -> ((1,0),x) 
    ||> List.fold folder 
    |> (function | 0,n -> Some n |l,_ -> None)


let splitTokens tokens s e = 
    let (before,after1) = tokens |> List.splitAt s
    let (inner,after2)  = after1  |> List.splitAt (e+1)
    let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
    let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
    (before,inner2,after3)

let styledToInlineElement tokens =
    tokens |> List.map (function | Styled a -> a| a -> failwithf "What? Should only be list of styled Tokens, %A" a)


let (|TryParseWith|_|) openDelim closeDelim tokens = 
    let sIdx   = findTokenIdx openDelim tokens
    let eIdx i = balancedCloseIdx openDelim closeDelim tokens i
    match (sIdx,Option.bind eIdx sIdx)  with
    | Some s,Some e -> splitTokens tokens s e |> Some
    | _ -> None


let (|TryParseWithPrefix|_|) openDelim closeDelim tokens = 
    let sIdx   = findTokenIdx openDelim tokens
    let eIdx i = balancedCloseIdx openDelim closeDelim tokens i
    match (sIdx,Option.bind eIdx sIdx)  with
    | Some s,Some e when s=0 -> splitTokens tokens s e |> Some
    | _ -> None

let (|TryParseUntil|_|) token tokens = 
    let sIdx   = findTokenIdx token tokens
    match sIdx with
    | Some s -> 
        let (before,after) = tokens |> List.splitAt s 
        let after2 = after |> (function t::rlst -> rlst |[]->[])
        (before,after2) |> Some
    | _ -> None


let rec parser tokens = 
    // Converts reamaining tokens into Text InlineElement
    // Will combine consecutive Text into a single Text
    let parseText tokens =
        let tokenToString token =
            match token with
            | Token2String str -> str

        let convertToText state token =
            let (accStr,out) = state
            match accStr,token with
            | "",Styled a   -> ("",Styled a::out)
            | s ,Styled a   -> ("",Styled a::Styled (Text s)::out)
            | s ,notStyled  -> 
                let strToken = notStyled |> tokenToString 
                (s+strToken,out)
        (("",[]),tokens) 
        ||> List.fold convertToText 
        |>  function
            | "",out -> out
            | s ,out -> Styled(Text s)::out
        |> List.rev

    // Helper function to convert tokens to Text Inline Element
    let toText tokens = 
        tokens |> parseText |> styledToInlineElement

    let rec parseCodeSpans tokens =
        let sIdx = tokens |> List.tryFindIndex (function | BacktickStr a -> true | _ -> false)

        // Find matching BacktickStr
        let eIdx i = tokens 
                     |> List.splitAt (i+1)
                     |> snd
                     |> List.tryFindIndex (function | t when t=tokens.[i] -> true | _-> false)
        match (sIdx,Option.bind eIdx sIdx)  with
        | Some s,Some e 
            -> let (before,inner,after) = splitTokens tokens s e
               before @ [inner|>toText|>CodeSpan|>Styled] @ (parseCodeSpans after)
        | Some s, _     
            -> let (before,after) = tokens |> List.splitAt (s+1)
               before @ (parseCodeSpans after)
        | _ -> tokens


    let rec parseLinksOrImg  tokens =
        let (|ParseText|_|) tokens = 
            match tokens with 
            | TryParseWith SBracketO SBracketC (before,inner,after)
                -> Some (before,
                         inner |> parser |> styledToInlineElement,
                         after)
            | _-> None

        let (|ParseTextImg|_|) tokens = 
            match tokens with
            | TryParseUntil Exclamation (before,ParseText(before2,inner,after))
                -> Some (before@before2,
                         inner,
                         after)
            | _ -> None

        let rec (|ParseLink|_|) tokens = 
            match tokens with
            | TryParseWithPrefix RBracketO RBracketC (before,inner,after)
                -> let maybeLink = inner |> stripWSHead |> stripWSTail
                   maybeLink 
                   |> List.tryFind(fun x-> x=Whitespace)
                   |> function 
                        // Do not want whitespace, only want continuous text
                       | None   -> Some (maybeLink|>toText,after)
                       | Some _ -> None
            | _ -> None

        let (|ParseTitle|_|) tokens = 
            match tokens with 
            | TryParseWithPrefix RBracketO RBracketC (before,inner,after)
                // Unlike, Title can be arbitaraitly long 
                -> Some (inner |>toText ,after)
            | _ -> None

        match tokens with
        // Try to Parse Images
        | ParseTextImg (before,text,ParseLink (link,after))
             -> let basicLinkInfo = {linkText=text;linkDest=link;linkTitle=None}
                match after with
                | ParseTitle (title,after2)   // Image with title
                    -> before @ [{basicLinkInfo with linkTitle=Some title}|>Image|>Styled] @ (parseLinksOrImg after2)
                                             // Image without title
                | _ -> before @ [basicLinkInfo|>Image|>Styled] @ (parseLinksOrImg after) 
        // Try to Parse Links
        | ParseText (before,text,ParseLink (link,after))
            -> let basicLinkInfo = {linkText=text;linkDest=link;linkTitle=None}
               match after with
               | ParseTitle (title,after2) // Link with title 
                    -> before @ [{basicLinkInfo with linkTitle=Some title}|>Link|>Styled] @ (parseLinksOrImg after2)
                                           // Link without title
                | _  -> before @ [basicLinkInfo|>Link|>Styled] @ (parseLinksOrImg after)
        | _  
            -> let idx =  match tokens with 
                          | FindTokenIdx Exclamation idx -> Some idx 
                          | FindTokenIdx SBracketO idx   -> Some idx
                          | _ -> None
               match idx with 
               | Some idx -> tokens 
                              |> List.splitAt (idx+1) 
                              |> (fun (before,after)->before @ (parseLinksOrImg after))
               | None -> tokens
               

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
                    |> List.map (
                        fun (od,cd) -> genericParser typeConst od cd)

        // Apply parsers sequentially
        (tokens,parsers) ||> List.fold (fun toks psr -> psr toks) 


    let rec parseBreaks tokens = 
       
        // Helper function to split up and strip whitespace
        let formatOutput pType before n tokens=
            tokens |> List.splitAt (n+1)
                   |> snd
                   |> fun after -> (before |> stripWSHead |>List.rev)  
                                    @ [pType|>Styled]
                                    @ (after |> stripWSHead |> parseBreaks)

        // Find the next Newline
        let eolIdx = tokens |> List.tryFindIndex (fun x->x =Newline)

        // Test for Softbreak and Hardbreak condition if Newline exists
        match tokens with
        | FindTokenIdx Newline n -> 
                match List.rev tokens.[..n-1] with 
                | []  -> failwithf "What? tokens empty cannot happen since eolIdx>=0"
                | [h] -> [h]
                | Backslash::before         
                        // Harbreak, Backslash                     
                    ->  formatOutput Hardbreak before n tokens 
                
                | Whitespace::Whitespace::before 
                        // Hardbreak, >=2 Whitespace                  
                    ->  formatOutput Hardbreak before n tokens
                | before                               

                    ->  formatOutput Softbreak before n tokens 
        | _   -> tokens       

    // Some generic LinkInfo
    let o = {linkDest=[];linkText=[];linkTitle=None}  
    // Delimiters for Strong and Emphasis  
    let strongDelims = [(StrongOpenAst,StrongCloseAst);(StrongOpenUnd,StrongCloseUnd)]
    let empDelims   =  [(EmpOpenAst,EmpCloseAst);(EmpOpenUnd,EmpCloseUnd)]


    // Main parsing execution
    tokens 
    |> parseCodeSpans 
    |> parseLinksOrImg 
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


//inlineParser "![img]()"
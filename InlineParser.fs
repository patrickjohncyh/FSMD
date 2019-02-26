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
    | t             -> 
        match (Map.tryFind t translation)  with
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

let findTokenIdx token tokens = 
    tokens |> List.tryFindIndex (fun x -> x = token)

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
               before @ [inner |>parseText|>styledToInlineElement |>CodeSpan|>Styled] @ (parseCodeSpans after)
        | Some s, _     
            -> let (before,after) = tokens |> List.splitAt (s+1)
               before @ (parseCodeSpans after)
        | _ -> tokens





    let rec parseLinksOrImg pType tokens =
        let (|ParseText|_|) flag tokens = 
            // Perform extra check if Parsing for Image (flag = true)
            match flag,findTokenIdx Exclamation tokens with
            | true, None -> None
            | _,_ ->
                let sIdx   = findTokenIdx SBracketO tokens
                let eIdx i = balancedCloseIdx SBracketO SBracketC tokens i
                match (sIdx,Option.bind eIdx sIdx)  with
                | Some s,Some e 
                    -> let (before,inner,after) = splitTokens tokens s e
                       Some (before,parser inner |> styledToInlineElement ,after)
                | _ -> None

        let rec (|ParseLink|_|) tokens = 
            let sIdx   = findTokenIdx RBracketO tokens
            let eIdx i = balancedCloseIdx RBracketO RBracketC tokens i
            match (sIdx,Option.bind eIdx sIdx)  with
            | Some s,Some e when s=0 
                ->  let (before,inner,after) = splitTokens tokens s e
                    printfn "%A" (before,after,inner)
                    let stripInner =  inner |> stripWSHead |> stripWSTail
                    stripInner 
                    |> stripWSHead
                    |> stripWSTail
                    |> List.tryFind(fun x-> x=Whitespace)
                    |> function 
                        //Do not want whitespace, since whitespace == more than 1 literal
                       | None   -> Some (stripInner |> parseText |> styledToInlineElement,after)
                       | Some _ -> None
            | _ -> None


        let (|ParseTitle|_|) tokens = 
            let sIdx   = findTokenIdx RBracketO tokens
            let eIdx i = balancedCloseIdx RBracketO RBracketC tokens i
            match (sIdx,Option.bind eIdx sIdx)  with
            | Some s,Some e when s=0-> let (before,inner,after) = splitTokens tokens s e
                                       Some (inner |> parseText |> styledToInlineElement,after)
            | _ -> None

         // Set constants based on parseType
        let (initToken,typeConst,flag) =   
            match pType with
            | Image _ -> Exclamation,Image,true
            | Link  _ -> SBracketO,Link,false
            | _       -> failwithf "What? parseLinkOrImg should only take Image or Link as parse type" 

        // parseLinksOrImg execution
        match tokens with
        | ParseText flag (before,text,ParseLink (link,after))
            -> match after with
               | ParseTitle (title,after2)  
                    // Link with title
                    -> before @ [{linkText=text;linkDest=link;linkTitle=Some title}|>typeConst|>Styled] @ (parseLinksOrImg pType after2)
               | _                          
                    // Link without title
                    -> before @ [{linkText=text;linkDest=link;linkTitle=None}|>typeConst|>Styled] @ (parseLinksOrImg pType after)
        | _ 
            ->  let sIdx = findTokenIdx initToken tokens
                match sIdx with
                | Some s -> 
                    let (before,after1) = tokens |> List.splitAt (s+1)
                    before @ (parseLinksOrImg pType after1)
                | None -> tokens




    let parseEmOrStrong pType tokens  =
        // Delimiters for Strong and Emphasis
        let strongDelims = [(StrongOpenAst,StrongCloseAst,Strong);(StrongOpenUnd,StrongCloseUnd,Strong)]
        let empDelims    = [(EmpOpenAst,EmpCloseAst,Emphasis);(EmpOpenUnd,EmpCloseUnd,Emphasis)]

        // Get Delimisters depending on parseType
        let delims = match pType with
                     | Strong    _  -> strongDelims
                     | Emphasis  _  -> empDelims
                     | _ -> failwithf "What? parseEmOrStrong should only take Strong or Emphasis"

        // Generic Emp of Strong Parser
        let rec innerFn (openTok,closeTok,typeConst) tokens = 
            // Form Specific type of Parser
            let innerFn = innerFn (openTok,closeTok,typeConst) 
            let sIdx    = tokens |> List.tryFindIndex (fun x->x =openTok)
            let eIdx i  = balancedCloseIdx openTok closeTok tokens i

            match (sIdx,Option.bind eIdx sIdx)  with
            | Some s,Some e 
                ->  let (before,inner,after) = splitTokens tokens s e
                    before @ [parser inner|> styledToInlineElement |>typeConst|>Styled] @ (innerFn after)
            | Some s, _    
                ->  let (before,after) = tokens |> List.splitAt (s+1)
                    before @ (innerFn after)
            | _ -> tokens

        // Make parsers based on delimiters
        let parsers = delims |> List.map innerFn

        // Apply parsers sequentially
        (tokens,parsers) ||> List.fold (fun pTokens p -> p pTokens) 





    let rec parseBreaks tokens = 
        // Find the next Newline
        let eolIdx = tokens |> List.tryFindIndex (fun x->x =Newline)

        // Helper function to split up and strip whitespace
        let performParse pType before n tokens=
            tokens |> List.splitAt (n+1)
                   |> snd
                   |> fun after -> (before |> stripWSHead |>List.rev)  
                                    @ [pType|>Styled]
                                    @ (after |> stripWSHead |> parseBreaks)

        // Test for Softbreak and Hardbreak condition if Newline exists
        match eolIdx with 
        | None   -> tokens
        | Some n -> 
            match List.rev tokens.[..n] with 
            | []  -> failwithf "What? tokens empty cannot happen since eolIdx>=0"
            | [h] -> [h]
            | Newline::Backslash::before                                // Harbreak, Backslash
                ->  performParse Hardbreak before n tokens             
            | Newline::Whitespace::Whitespace::before                   // Hardbreak, >=2 Whitespace
                ->  performParse Hardbreak before n tokens
            | Newline::Whitespace::before                               // Softbreak, 1 Whitespace
                ->  performParse Softbreak before n tokens               
            | _ -> tokens                                               // No break
                   |> List.splitAt (n+1)
                   |> fun (before,after) ->  before @ (parseBreaks after)


                          
    // Some generic LinkInfo
    let o = {linkDest=[];linkText=[];linkTitle=None}    
    
    // Main parsing execution
    tokens 
    |> parseCodeSpans 
    |> parseLinksOrImg (Image o)
    |> parseLinksOrImg (Link  o) 
    |> parseEmOrStrong (Strong   [])
    |> parseEmOrStrong (Emphasis [])
    |> parseBreaks
    |> parseText

   
// Top level function
let inlineParser inputString =
    let tokenList = inlineTokeniser inputString
    tokenList 
    |> parser 
    |> styledToInlineElement


//inlineParser "[test](one two)"
//inlineParser "[test](google.com)"
//inlineParser "[Test link](  www.google.com  )"
//inlineTokeniser "[test](one two)"
open System.Text.RegularExpressions

type Token = 
    | SBracketO
    | SBracketC
    | RBracketO
    | RBracketC
    | QuoteMark
    | Whitespace
    | Exclamation
    | Asterisk
    | EmpOpen
    | EmpClose
    | StrongOpen
    | StrongClose
    | Underscore
    | LessThan
    | GreaterThan
    | Escaped of string
    | Text    of string
    | BacktickStr of int
    | Styled  of InlineElement

and InlineElement = 
    | Link  of LinkInfo
    | Image of LinkInfo
    | Plain of Token list
    | CodeSpan  of Token list
    | Strong of Token list
    | Emphasis of Token list

and LinkInfo = {linkText:Token list;
                linkDest: Token list; 
                linkTitle : Token list option}

let (|RegexPrefix|_|) pat txt =
    let m = Regex.Match(txt,"^"+ pat)
    match m.Success with
    | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
    | false -> None
    
//########################## Tokenizer ##########################   
let rec inlineTokeniser txt = 
    match txt with
    | "" -> []
    | _  ->
        let (newToken,remain) = 
            match txt with
            | RegexPrefix "\\\[\\!-\\/\\:-\\@\\[-\\`\\{\\~]" (str,remain) -> ((Escaped str),remain)
            | RegexPrefix "\*\*\("         (_,remain)   -> (StrongOpen,remain)
            | RegexPrefix "\)\*\*"         (_,remain)   -> (StrongClose,remain)
            | RegexPrefix "\*\("         (_,remain)   -> (EmpOpen,remain)
            | RegexPrefix "\)\*"         (_,remain)   -> (EmpClose,remain)
            | RegexPrefix "\["          (_,remain)    -> (SBracketO,remain)
            | RegexPrefix "\]"          (_,remain)    -> (SBracketC,remain)
            | RegexPrefix "\("          (_,remain)    -> (RBracketO,remain)
            | RegexPrefix "\)"          (_,remain)    -> (RBracketC,remain)
            | RegexPrefix "_"           (_,remain)    -> (Underscore,remain)
            | RegexPrefix "\*"         (_,remain)   ->  (Asterisk,remain)
            | RegexPrefix "\!"         (_,remain)   ->  (Exclamation,remain)
            | RegexPrefix "[\"]"         (_,remain)   ->  (QuoteMark,remain)
            | RegexPrefix "[`]+"        (str,remain)  -> (BacktickStr str.Length,remain)
            | RegexPrefix "[\s]"       (_,remain)    -> (Whitespace,remain)
            | RegexPrefix "[A-Za-z0-9\.]*" (str,remain) -> ((Text str),remain)
         //   | RegexPrefix "[^`*\s\[\]\(\)\\\]*" (str,remain) -> ((Text str),remain)
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


let rec parser tokens = 

    let rec parseCodeSpans tokens = 
        let sIdx = tokens   |> List.tryFindIndex (function | BacktickStr a -> true | _ -> false)
        let eIdx i = tokens 
                     |> List.splitAt (i+1)
                     |> snd
                     |> List.tryFindIndex (function | t when t=tokens.[i] -> true | _-> false)
        match (sIdx,Option.bind (eIdx) sIdx)  with
        | Some s,Some e -> let (before,inner,after) = splitTokens tokens s e
                           before @ [inner|>CodeSpan|>Styled] @ (parseCodeSpans after)
        | Some s, _     -> let (before,after) = tokens |> List.splitAt (s+1)
                           before @ (parseCodeSpans after)
        | _ -> tokens

    let rec parseLinksOrImg pType tokens =
        let (|ParseText|_|) flag tokens = 
            match flag,findTokenIdx Exclamation tokens with
            | true, None -> None
            | _,_ ->
                let sIdx   = findTokenIdx SBracketO tokens
                let eIdx i = balancedCloseIdx SBracketO SBracketC tokens i
                match (sIdx,Option.bind (eIdx) sIdx)  with
                | Some s,Some e -> let (before,inner,after) = splitTokens tokens s e
                                   Some (before,parser inner,after)
                | _ -> None

        let rec (|ParseLink|_|) tokens = 
            let sIdx   = findTokenIdx RBracketO tokens
            let eIdx i = balancedCloseIdx RBracketO RBracketC tokens i
            match (sIdx,Option.bind (eIdx) sIdx)  with
            | Some s,Some e when s=0 ->  let (before,inner,after) = splitTokens tokens s e
                                         if (before.Length < 2) then Some (inner,after) else None //1 or less items
            | _ -> None

        let (|ParseTitle|_|) tokens = 
            let sIdx   = findTokenIdx RBracketO tokens
            let eIdx i = balancedCloseIdx RBracketO RBracketC tokens i
            match (sIdx,Option.bind (eIdx) sIdx)  with
            | Some s,Some e when s=0-> let (before,inner,after) = splitTokens tokens s e
                                       Some (inner,after)
            | _ -> None

        let (initToken,typeConst,flag) =   
            match pType with
            | Image _ -> Exclamation,Image,true
            | Link  _ -> SBracketO,Link,false
            | _       -> failwithf "What? parseLinkOrImg should only take Image or Link as parse type" 


        match tokens with
        | ParseText flag (before,text,ParseLink (link,after))
            -> match after with
               | ParseTitle (title,after2)  // Has title
                    -> before @ [{linkText=text;linkDest=link;linkTitle=Some title}|>typeConst|>Styled] @ (parseLinksOrImg pType after2)
               | _                          // No title
                    -> before @ [{linkText=text;linkDest=link;linkTitle=None}|>typeConst|>Styled] @ (parseLinksOrImg pType after)
        | _ 
            ->  let sIdx = findTokenIdx initToken tokens
                match sIdx with
                | Some s -> 
                    let (before,after1) = tokens |> List.splitAt (s+1)
                    before @ (parseLinksOrImg pType after1)
                | None -> tokens
                


    let rec parseEmphasis tokens = 
       let sIdx = tokens   |> List.tryFindIndex (fun x->x =EmpOpen)
       let eIdx i = balancedCloseIdx EmpOpen EmpClose tokens i
       match (sIdx,Option.bind (eIdx) sIdx)  with
       | Some s,Some e ->  let (before,inner,after) = splitTokens tokens s e
                           before @ [parser inner|>Emphasis|>Styled] @ (parseEmphasis after)

       | Some s, _     ->  let (before,after) = tokens |> List.splitAt (s+1)
                           before @ (parseCodeSpans after)
       | _ -> tokens

    let rec parseStrong tokens = 
       let sIdx = tokens   |> List.tryFindIndex (fun x->x =StrongOpen)
       let eIdx i = balancedCloseIdx StrongOpen StrongClose tokens i
       match (sIdx,Option.bind (eIdx) sIdx)  with
       | Some s,Some e -> let(before,inner,after) = splitTokens tokens s e
                          before @ [parser inner|>Strong|>Styled] @ (parseStrong after)
       | Some s, _     -> let (before,after1) = tokens |> List.splitAt (s+1)
                          before @ (parseCodeSpans after1)
       | _ -> tokens

    let o = {linkDest=[];linkText=[];linkTitle=None}    //some generic LinkInfo
    tokens 
    |> parseCodeSpans 
    |> parseLinksOrImg (Image o)
    |> parseLinksOrImg (Link  o) 
    |> parseStrong 
    |> parseEmphasis


let tokenList = inlineTokeniser "![*[(emphasis)* within a link](www.google.com)"//"[hope this works](lol)"

parser tokenList 




(*
// Auto Link :  "< Absolute URI >"
// Absolute URI : "Scheme : ASCII (Exlcuding Ws < >)"   
let (|AutoLink|_|) txt = 
    let scheme = "[A-Za-z][A-Za-z0-9\+\-\.]{1,31}"
    let uri    = "[^<>\\s]*"
    let absoluteUri = scheme + ":" + uri
    let uriPattern    = "<" + absoluteUri + ">"
    matchRegex uriPattern txt 
*)


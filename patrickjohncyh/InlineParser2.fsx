open System.Text.RegularExpressions

type Token = 
    | SBracketO
    | SBracketC
    | RBracketO
    | RBracketC
    | QuoteMark
    | Whitespace
    | Asterisk
    | EmpOpen
    | EmpClose
    | StrongOpen
    | StrongClose
    | Underscore
    | Escaped of string
    | Text    of string
    | BacktickStr of int
    | Styled  of InlineElement


and InlineElement = 
    | Link  of LinkInfo
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
    
let (|Inner|_|) txt = 
    match txt with 
    | ""   -> None
    | text -> Some text.[1..text.Length-2]

let (|Or|_|) ap1 ap2 =
    let innerFn txt =
        match (ap1 txt) with
        | Some result -> Some result
        | None ->
            match (ap2 txt) with
            | Some result -> Some result
            | None  -> None         
    innerFn

let (<|>) = (|Or|_|)

let (|Chain|_|) listOfAP tokenList= 
    let folder state ap = 
        match state with
        | None -> None
        | Some (result,remain) ->
            match ap remain with
            | Some (v,r) -> Some (v::result,r)
            | None -> None

    (Some ([],tokenList),listOfAP) ||> List.fold folder |> Option.bind(fun (v,r)-> Some (List.rev v,r))
    
let (|InnerTokensSB|_|) tokenList =
    let folder state token =
        match state with
        | (acc,remain,0) -> (acc,token::remain,0)
        | (acc,remain,n) ->
            match token with
            | SBracketO             -> (token::acc,remain,n+1)
            | SBracketC when(n-1=0) -> (acc,remain,0)
            | SBracketC             -> (SBracketC::acc,remain,n-1)
            | _                     -> (token::acc,remain,n)

    match tokenList with
    | SBracketO::rlst -> 
        let tryParse = (([],[],1),rlst) ||> List.fold folder
        match tryParse with
        | (acc,remain,0) -> (List.rev acc,List.rev remain) |> Some
        | _ -> None
    | _ -> None

let (|InnerTokensRB|_|) tokenList =
    let folder state token =
        match state with
        | (acc,remain,0) -> (acc,token::remain,0)
        | (acc,remain,n) ->
            match token with
            | RBracketO             -> (token::acc,remain,n+1)
            | RBracketC when(n-1=0) -> (acc,remain,0)
            | RBracketC             -> (RBracketC::acc,remain,n-1)
            | _                     -> (token::acc,remain,n)

    match tokenList with
    | RBracketO::rlst -> 
        let tryParse = (([],[],1),rlst) ||> List.fold folder
        match tryParse with
        | (acc,remain,0) -> (List.rev acc,List.rev remain) |> Some
        | _ -> None
    | _ -> None

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
            | RegexPrefix "[\"]"         (_,remain)   ->  (QuoteMark,remain)
            | RegexPrefix "[`]+"        (str,remain)  -> (BacktickStr str.Length,remain)
            //| RegexPrefix "\"[^\"]*\""  (str,remain)  -> ((DQuoted str),remain)
            //| RegexPrefix "\'[^\']*\'"  (str,remain)  -> ((SQuoted str),remain)
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


let rec parser tokens = 

    let rec parseCodeSpans tokens = 
        let sIdx = tokens   |> List.tryFindIndex (function | BacktickStr a -> true | _ -> false)
        let eIdx i = tokens 
                     |> List.splitAt (i+1)
                     |> snd
                     |> List.tryFindIndex (function | t when t=tokens.[i] -> true | _-> false)
        match (sIdx,Option.bind (eIdx) sIdx)  with
        | Some s,Some e ->
            let (before,after1) = tokens |> List.splitAt s
            let (inner,after2)  = after1  |> List.splitAt (e+1)
            let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
            let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
            //printfn "%A %A %A" before inner2 after3
            before @ [inner2|>CodeSpan|>Styled] @ (parseCodeSpans after3)
        | Some s, _ ->
            let (before,after1) = tokens |> List.splitAt (s+1)
            before @ (parseCodeSpans after1)
        | _ -> tokens

    let rec parseLinks tokens = 

        let parseText tokens = 
            let folder state tok = 
                let (lvl,idx) = state
                let nIdx = idx+1  
                match lvl,tok with 
                | 0,_         -> (lvl  ,idx)
                | 1,SBracketC -> (lvl-1,idx)          
                | n,SBracketC -> (lvl-1,nIdx)
                | n,SBracketO -> (lvl+1,nIdx)
                | n,_         -> (lvl  ,nIdx)
            let sIdx   = tokens |> List.tryFindIndex (fun x -> x = SBracketO)
            let eIdx i = tokens
                         |> List.splitAt (i+1)
                         |> snd
                         |> fun x -> ((1,0),x) 
                         ||> List.fold folder 
                         |> (function | 0,n -> Some n |l,_ -> None)
            match (sIdx,Option.bind (eIdx) sIdx)  with
            | Some s,Some e ->
                let (before,after1) = tokens |> List.splitAt s
                let (inner,after2)  = after1  |> List.splitAt (e+1)
                let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
                let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
                //printfn "%A %A %A" before inner2 after3
                Some (before,inner2,after3)
            | _ -> None

        let rec parseLink tokens = 
            let folder state tok = 
                let (lvl,idx) = state
                let nIdx = idx+1  
                match lvl,tok with 
                | 0,_         -> (lvl  ,idx)
                | 1,RBracketC -> (lvl-1,idx)          
                | n,RBracketC -> (lvl-1,nIdx)
                | n,RBracketO -> (lvl+1,nIdx)
                | n,_         -> (lvl  ,nIdx)
            let sIdx   = tokens |> List.tryFindIndex (fun x -> x = RBracketO)
            let eIdx i = tokens
                         |> List.splitAt (i+1)
                         |> snd
                         |> fun x -> ((1,0),x) 
                         ||> List.fold folder 
                         |> (function | 0,n -> Some n |l,_ -> None)
            match (sIdx,Option.bind (eIdx) sIdx)  with
            | Some s,Some e when s=0->      //deviation from previous use of similar code, maybe can compress below part into another function
                let (before,after1) = tokens |> List.splitAt s
                let (inner,after2)  = after1  |> List.splitAt (e+1)
                let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
                let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
                //printfn "%A %A %A" before inner2 after3
                if (inner2.Length < 2) then Some (before,inner2,after3) else None //1 or less items
            | _ -> None

        let parseTitle tokens = 
            printfn "title :%A" tokens
            let folder state tok = 
                let (lvl,idx) = state
                let nIdx = idx+1  
                match lvl,tok with 
                | 0,_         -> (lvl  ,idx)
                | 1,RBracketC -> (lvl-1,idx)          
                | n,RBracketC -> (lvl-1,nIdx)
                | n,RBracketO -> (lvl+1,nIdx)
                | n,_         -> (lvl  ,nIdx)
            let sIdx   = tokens |> List.tryFindIndex (fun x -> x = RBracketO)
            let eIdx i = tokens
                         |> List.splitAt (i+1)
                         |> snd
                         |> fun x -> ((1,0),x) 
                         ||> List.fold folder 
                         |> (function | 0,n -> Some n |l,_ -> None)
            match (sIdx,Option.bind (eIdx) sIdx)  with
            | Some s,Some e when s=0->      //deviation from previous use of similar code, maybe can compress below part into another function
                let (before,after1) = tokens |> List.splitAt s
                let (inner,after2)  = after1  |> List.splitAt (e+1)
                let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
                let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
                //printfn "%A %A %A" before inner2 after3
                Some (before,inner2,after3)
            | _ -> None

        let linkDetails = match parseText tokens with
                          | None -> None
                          | Some (before,text,after) ->
                            match parseLink after  with
                            | None -> None
                            | Some (_,link,after2) -> Some (before,text,link,after2)

        match linkDetails with 
        | None -> 
            let sIdx = tokens |> List.tryFindIndex (fun x -> x = SBracketO)
            match sIdx with
            | None -> tokens
            | Some s ->
                let (before,after1) = tokens |> List.splitAt (s+1)
                before @ (parseLinks after1)
        | Some (before,text,link,after) ->
            let (title,remain) = match parseTitle after with
                                 | Some (_,title,remain) ->  (Some title,remain) //has title
                                 | None                  ->  (None,after)   //no title
            before @ [{linkText=text;linkDest=link;linkTitle=title}|>Link|> Styled] @ (parseLinks remain)

    let rec parseEmphasis tokens = 
       let folder state tok = 
           let (lvl,idx) = state
           let nIdx = idx+1  
           match lvl,tok with 
           | 0,_        -> (lvl  ,idx)
           | 1,EmpClose -> (lvl-1,idx)          
           | n,EmpClose -> (lvl-1,nIdx)
           | n,EmpOpen  -> (lvl+1,nIdx)
           | n,_        -> (lvl  ,nIdx)
       let sIdx = tokens   |> List.tryFindIndex (fun x->x =EmpOpen)
       let eIdx i = tokens
                    |> List.splitAt (i+1)
                    |> snd
                    |> fun x -> ((1,0),x) 
                    ||> List.fold folder 
                    |> (function | 0,n -> Some n |l,_ -> None)
       match (sIdx,Option.bind (eIdx) sIdx)  with
       | Some s,Some e ->
           let (before,after1) = tokens |> List.splitAt s
           let (inner,after2)  = after1  |> List.splitAt (e+1)
           let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
           let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
           //printfn "%A %A %A" before inner2 after3
           before @ [parser inner2|>Emphasis|>Styled] @ (parseEmphasis after3)
       | Some s, _ ->
           let (before,after1) = tokens |> List.splitAt (s+1)
           before @ (parseCodeSpans after1)
       | _ -> tokens

    let rec parseStrong tokens = 
       let folder state tok = 
           let (lvl,idx) = state
           let nIdx = idx+1  
           match lvl,tok with 
           | 0,_        -> (lvl  ,idx)
           | 1,StrongClose -> (lvl-1,idx)          
           | n,StrongClose -> (lvl-1,nIdx)
           | n,StrongOpen  -> (lvl+1,nIdx)
           | n,_        -> (lvl  ,nIdx)
       let sIdx = tokens   |> List.tryFindIndex (fun x->x =StrongOpen)
       let eIdx i = tokens
                    |> List.splitAt (i+1)
                    |> snd
                    |> fun x -> ((1,0),x) 
                    ||> List.fold folder 
                    |> (function | 0,n -> Some n |l,_ -> None)
       match (sIdx,Option.bind (eIdx) sIdx)  with
       | Some s,Some e ->
           let (before,after1) = tokens |> List.splitAt s
           let (inner,after2)  = after1  |> List.splitAt (e+1)
           let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
           let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
           //printfn "%A %A %A" before inner2 after3
           before @ [parser inner2|>Strong|>Styled] @ (parseStrong after3)
       | Some s, _ ->
           let (before,after1) = tokens |> List.splitAt (s+1)
           before @ (parseCodeSpans after1)
       | _ -> tokens
       

    tokens |> parseCodeSpans |> parseLinks |> parseStrong |> parseEmphasis


let tokenList = inlineTokeniser "*([links]())* *(`*(conde)* span`)*"

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


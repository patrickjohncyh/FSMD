open System.Text.RegularExpressions

type Token = 
    | SBracketO
    | SBracketC
    | RBracketO
    | RBracketC
    | QuoteMark
    | Whitespace
    | Asterisk
    | Underscore
    | SQuoted of string
    | DQuoted of string
    | Escaped of string
    | Text    of string
    | BacktickStr of int

   
type InlineElement = 
    | Link  of LinkInfo
    | Plain of Token list
    | CodeSpan  of Token list
    | Strong

and LinkInfo = {linkText:InlineElement list;linkDest: InlineElement; linkTitle : InlineElement option }

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

let rec noLeadingWhitespace tokenList = 
    match tokenList with
    | []      -> []
    | Whitespace::rlst ->  noLeadingWhitespace rlst
    | _ -> tokenList


//##########################  Begin Parser  ##########################

let rec (|InlineParser|_|) tokenList = 

//########################## Title ##########################
    let (|LinkDestTitle|_|) tokenList = 
        let strpList = tokenList|> List.rev |> noLeadingWhitespace |> List.rev

        let splitIdx = strpList |> List.tryFindIndex (function | DQuoted x -> true 
                                                               | SQuoted x -> true 
                                                               | _ -> false)
        let destTitle =
            match splitIdx with
            | Some idx when idx=strpList.Length-1 ->
                List.splitAt idx strpList |> (fun (a,b) -> (a,Some (Plain b))) |> Some
            | None -> (strpList,None) |> Some
            | _    -> None

        match destTitle with
        | None -> None
        | Some (dest,linkTitle) ->
            let linkDest = dest |> noLeadingWhitespace |> List.rev 
                                |> noLeadingWhitespace |> List.rev
            let hasWhitespace = linkDest |> List.tryFind (fun t->t=Whitespace)
            match hasWhitespace with
            | None   -> Some (Plain linkDest,linkTitle)
            | _      -> None


    let (|ParseLink|_|) tokenList = 
        let toMatch = [(|InnerTokensSB|_|);(|InnerTokensRB|_|)]
        match tokenList with
        | Chain toMatch (result,remain) ->
            let linkText =
                match result.[0] with
                | InlineParser v -> v
                | _ -> [Plain [Text ""]]
            match result.[1] with
                | LinkDestTitle (linkDest,linkTitle) -> 
                    (Link {linkText=linkText;
                           linkDest=linkDest;
                           linkTitle=linkTitle },remain) |> Some
                | _ -> None
        | _ -> None
//########################## Code Span ##########################
    let (|ParseCodeSpan|_|) tokenList = 
        match tokenList with
        | (BacktickStr a)::rlst ->
            match List.tryFindIndex (fun x->x=(BacktickStr a)) rlst with
            | None -> None
            | Some idx -> 
               rlst 
               |> List.splitAt idx //[CodeSpanInner,BackTick+Remain] 
               |> (fun (a,b)-> 
                    let remain = match b with
                                 | [] -> []
                                 | h::tail -> tail
                    (CodeSpan a,remain) |> Some)
        | _ -> None

//##########################  Emphasis  ##########################
    let rec (|OpenEmphasis|_|) tokenList = 
        match tokenList with
        | Asterisk::remain->
            match remain with
            | OpenEmphasis (v,r) -> (Asterisk::v,r) |> Some
            | _ -> None   
        | Whitespace::_-> None
        | _ -> ([],tokenList) |> Some

    let rec getNextClosing (tokenList,inner,len) =
        match tokenList with
        | Asterisk::Asterisk::remain -> getNextClosing (remain,inner,len+2)
        | Asterisk::remain           -> (len+1,List.rev inner,remain)
        | [token]                    -> (len,inner,tokenList )
        | token::remain              -> getNextClosing (remain,(token::inner),len)
        | []                         -> (len,inner,tokenList )

   (*let rec (|CloseEmphasis|_|) openEmph tokenList =
        let (closeLen,inner,remain) = getNextClosing (tokenList,[],0)
        match closeLen with
        | 0 -> None
        | _ ->
            let validLen  = min closeLen (List.length openEmph)
            let excess    = List.splitAt (closeLen-validLen) openEmph |> fst
            let strong    = [1..validLen/2]
            let em        = [1..validLen%2]
            match excess with
            | [] -> //do nothing
            | _  -> //try to match with reamining
                match remain with 
                | CloseEmphasis excess -> //expect inlineElement list * remain
                | _ ->  //matched with nothing, excess jsut plain, remain remians *)



    // Need to find the furtherst ?
    let (|ParseEmphasis|_|) tokenList =
        match tokenList with 
        | OpenEmphasis (emph,remain) ->
            let maskFolder state token = 
                let (inflag,prevToken,mask) = state
                let newFlag=  match (prevToken,token) with
                              | Asterisk,Asterisk     -> inflag
                              | Whitespace,Asterisk   -> false
                              | _,Asterisk            -> true
                              | _,_                   -> false
                let newMask = if newFlag then Asterisk::mask else Whitespace::mask
                (newFlag,token,newMask)

            let mask = ((false,Asterisk,[]),remain) ||> List.fold maskFolder 
                                                    |> (fun (a,b,c) -> c) 
                                                    |> List.rev
            let folder state _ =
                printfn "%A" state 
                let (n,acc,remain,mask) = state
                let nextAst  = mask    |> List.tryFindIndex (fun a -> a=Asterisk)  
                let isDouble = nextAst 
                               |> Option.bind (fun idx -> List.tryItem (idx+1) remain) 
                               |> Option.bind (fun item-> if item=Asterisk then Some item else None )
                let size,idx =  match n,nextAst,isDouble with 
                                | 0,_,_                -> 0,0
                                | 1,Some idx,_         -> 1,idx
                                | n,Some idx,Some _    -> 2,idx
                                | n,Some idx,_         -> 1,idx
                                | _,None,_             -> 0,0
                match size with 
                | 0 -> (n,acc,remain,mask)
                | size ->        
                    let newAcc,newRemain = List.splitAt idx remain |> fun(a,b)-> (size,a),b.[size..] 
                    let newMask       = List.splitAt idx mask   |> snd |> fun b -> b.[size..]
                    (n-size,newAcc::acc,newRemain,newMask)
            let splits,remain = ((emph.Length,[],remain,mask),emph) 
                                ||> List.fold folder 
                                 |> fun(a,b,c,d) -> (List.rev b,c)

           (_,splits) ||> List.fold (fun (state,tokList) ->
                                        let   )

            (splits,remain) |> Some
        | _ -> None


    


//##########################   Plain   ##########################
    let (|ParsePlain|_|) tokenList =
        let predicate tok = 
            match tok with
            | Text a     -> false
            | Whitespace -> false
            | Escaped  a -> false 
            | _ -> true
        match tokenList with
        | [] -> None
        | next::remain -> (Plain [next], remain) |> Some
 

//########################## Execution ##########################            
    let parseStep = 
        match tokenList with
        | [] -> None
        | ParseLink     (value,remain) -> Some (value,remain)
        | ParseCodeSpan (value,remain) -> Some (value,remain)
        //| ParseEmphasis (value,remain) -> Some (value,remain)
        | ParsePlain    (value,remain) -> Some (value,remain)
        | _ -> None

    match parseStep with
    | Some (value,remain) ->
        match remain with
        | InlineParser v2 -> List.concat [[value];v2] |> Some
        | _ -> Some [value]
    | None -> None
//########################## End Parser ##########################   





//########################## Tokenizer ##########################   
let rec inlineTokeniser txt = 
    match txt with
    | "" -> []
    | _  ->
        let (newToken,remain) = 
            match txt with
            | RegexPrefix "\\\[\\!-\\/\\:-\\@\\[-\\`\\{\\~]" (str,remain) -> ((Escaped str),remain)
            | RegexPrefix "\["          (_,remain)    -> (SBracketO,remain)
            | RegexPrefix "\]"          (_,remain)    -> (SBracketC,remain)
            | RegexPrefix "\("          (_,remain)    -> (RBracketO,remain)
            | RegexPrefix "\)"          (_,remain)    -> (RBracketC,remain)
            | RegexPrefix "_"           (_,remain)    -> (Underscore,remain)
            | RegexPrefix "[*]"         (_,remain)   ->  (Asterisk,remain)
            | RegexPrefix "[`]+"        (str,remain)  -> (BacktickStr str.Length,remain)
            | RegexPrefix "\"[^\"]*\""  (str,remain)  -> ((DQuoted str),remain)
            | RegexPrefix "\'[^\']*\'"  (str,remain)  -> ((SQuoted str),remain)
            | RegexPrefix "[\s]"       (_,remain)    -> (Whitespace,remain)
            | RegexPrefix "[A-Za-z0-9\.]*" (str,remain) -> ((Text str),remain)
         //   | RegexPrefix "[^`*\s\[\]\(\)\\\]*" (str,remain) -> ((Text str),remain)
        match (inlineTokeniser remain) with
        | [] -> [newToken]
        | tokenList -> newToken :: tokenList

let tokenList = inlineTokeniser "*lol `rekt lol*` lol"

match tokenList  with 
| InlineParser result -> Some result
| _ -> None



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



(|ParseEmphasis|_|) tokenList



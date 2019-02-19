open System.Text.RegularExpressions

type Token = 
    | SBracketO
    | SBracketC
    | RBracketO
    | RBracketC
    | QuoteMark
    | Whitespace
    | Escaped of string
    | Text of string

   
type InlineElement = 
    | Link  of LinkInfo
    | Plain of Token list

and LinkInfo = {linkText:InlineElement list;linkDest: InlineElement}
       
let (|RegexPrefix|_|) pat txt =
    // Match from start, ignore whitespace
    let m = Regex.Match(txt, "^" + pat) //"^[\\s]*" + pat + "[\\s]*")    
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

let rec (|InlineParser|_|) tokenList = 

    let (|LinkDest|_|) tokenList = 
        let linkDest = tokenList 
                       |> noLeadingWhitespace 
                       |> List.rev 
                       |> noLeadingWhitespace 
                       |> List.rev
        let hasWhitespace = linkDest |> List.exists (fun t->t=Whitespace)
        match hasWhitespace with
        | false -> Some (Plain linkDest)
        | true  -> None //need to check for " " optional title

    // [...](..)
    let (|ParseLink|_|) tokenList = 
        let toMatch = [(|InnerTokensSB|_|);(|InnerTokensRB|_|)]
        match tokenList with
        | Chain toMatch (result,remain) ->
            let linkText =
                match result.[0] with
                | InlineParser v -> v
                | _ -> [Plain [Text ""]]
            let linkDest = 
                match result.[1] with
                | LinkDest v -> Some v
                | _ -> None
            printfn "lT %A lD %A" result.[0] result.[1]
            printfn "lT %A lD %A" linkText linkDest
            match linkText,linkDest with
            | linkText, Some linkDest -> (Link {linkText=linkText;linkDest=linkDest},remain) |> Some
            | _,None ->  None
        | _ -> None

    let (|ParseText|_|) tokenList =
        let predicate tok = 
            match tok with
            | Text a    -> false
            | Whitespace -> false
            | Escaped  a -> false
            | _ -> true
        match tokenList with
        | [] -> None
        | _  ->
            let endIdx = List.tryFindIndex predicate tokenList 
            match endIdx with 
            | Some idx -> Some (Plain tokenList.[0..idx],tokenList.[idx+1..])
            | None     -> Some (Plain tokenList,[]) 
            
    let parseStep = 
        match tokenList with
        | ParseLink (value,remain) -> Some (value,remain)
        | ParseText (value,remain) -> Some (value,remain)
        | _ -> None

    match parseStep with
    | Some (value,remain) ->
        match remain with
        | InlineParser v2 -> List.concat [[value];v2] |> Some
        | _ -> Some [value]
    | None -> None


let rec inlineTokeniser txt = 
    match txt with
    | "" -> []
    | _  ->
        let (newToken,remain) = 
            match txt with
            | RegexPrefix "\[" (_,remain)            -> (SBracketO,remain)
            | RegexPrefix "\]" (_,remain)            -> (SBracketC,remain)
            | RegexPrefix "\(" (_,remain)            -> (RBracketO,remain)
            | RegexPrefix "\)" (_,remain)            -> (RBracketC,remain)
            | RegexPrefix "[\s]+" (_,remain)         -> (Whitespace,remain)
            | RegexPrefix "\\\[\\!-\\/\\:-\\@\\[-\\`\\{\\~]" (str,remain) -> ((Escaped str),remain)
            | RegexPrefix "[^\s\[\]\(\)\\\]*" (str,remain) -> ((Text str),remain)
        match (inlineTokeniser remain) with
        | [] -> [newToken]
        | tokenList -> newToken :: tokenList

let tokenList = inlineTokeniser "[ab](a)[a[]b](a)"//"[hello world](w\?ww.google.com)[hello world](www.goxogle.xxom)"
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


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

and LinkInfo = {linkText:InlineElement list;linkDest: Token list}
       
let (|RegexPrefix|_|) pat txt =
    // Match from start, ignore whitespace
    let m = Regex.Match(txt, "^" + pat) //"^[\\s]*" + pat + "[\\s]*")    
    match m.Success with
    | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
    | false -> None
    
let matchRegex = (|RegexPrefix|_|)

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

    (Some ([],tokenList),listOfAP) ||> List.fold folder
    
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

    let inlineParser = (|InlineParser|_|) 

    let (|LinkDest|_|) tokenList = 
        let linkDest = tokenList 
                       |> noLeadingWhitespace 
                       |> List.rev 
                       |> noLeadingWhitespace 
                       |> List.rev
        let hasWhitespace = linkDest |> List.exists (fun t->t=Whitespace)
        match hasWhitespace with
        | false -> Some linkDest
        | true  -> None //need to check for " " 
        //strip leading and training whitespace and check no whitespace remaining -> link only or maybe with title

    // [...](..)
    let (|ParseLink|_|) tokenList = 
        let toMatch = [(|InnerTokensSB|_|);(|InnerTokensRB|_|)]
        match tokenList with
        | Chain toMatch (result,remain) ->
            let textToks = result.[0] // Pasre into inlineElements
            let destToks = result.[1] // Need to verify format
            

        | _ -> None
         
       (* match innerTokensSB tokenList with
        | Some (linkText,remain) -> 
            match innerTokensRB remain with
            | Some (linkDest,remain) ->
                let linkTextEl = 
                    match linkText with 
                    | InlineParser (v,r) -> Some v 
                    | _ -> None //?
                match linkDest with //ensure if goodl linkdest
                | LinkDest value -> 
                    (Link {linkText=linkTextEl;linkDest=value},remain)
                    |> Some   
                | _ -> None
            | None -> None
        | None -> None*)

    let (|ParseText|_|) tokenList = 
        let predicate tok = 
            match tok with
            | Text a  -> false
            | Whitespace -> false
            | Escaped  a -> false
            | _ -> true
        match tokenList with
        | [] -> None
        | _  ->
            let endIdx = tokenList |> List.tryFindIndex predicate
            //printfn "%A" endIdx
            match endIdx with 
            | Some idx -> Some (Plain tokenList.[0..idx],tokenList.[idx+1..])
            | None     -> Some (Plain tokenList,[]) //consume everything?
            

    match tokenList with
    | ParseLink (value,remain) -> Some (value,remain)
    //| ParseText (value,remain) -> Some (value,remain)
    | _ -> None
    
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

let tokenList = inlineTokeniser "[hello world](w\?ww.google.com)[hello world](www.goxogle.xxom)"
match tokenList  with 
| InlineParser result -> Some result
| _ -> None




(*
let (|BackslashEscape|_|) txt =
    let escapePattern = "\\\[\\!-\\/\\:-\\@\\[-\\`\\{\\~]"
    matchRegex escapePattern txt

// To Add : leading and trailing spaces and line endings removed,
//           and whitespace collapsed to single spaces.
let (|CodeSpan|_|) txt =
    let openingBackTickPattern = "[`]+" 
    match txt with
    | RegexPrefix openingBackTickPattern (value,remain) ->
        match remain.IndexOf(value) with
        | -1  -> None
        | idx -> 
            let subsValue  = remain.Remove(idx)
            let subsRemain = remain.[idx+value.Length..]
            Some (subsValue,subsRemain)    
    | _ -> None
    

// Auto Link :  "< Absolute URI >"
// Absolute URI : "Scheme : ASCII (Exlcuding Ws < >)"   
let (|AutoLink|_|) txt = 
    let scheme = "[A-Za-z][A-Za-z0-9\+\-\.]{1,31}"
    let uri    = "[^<>\\s]*"
    let absoluteUri = scheme + ":" + uri
    let uriPattern    = "<" + absoluteUri + ">"
    matchRegex uriPattern txt    *)

(*
//Chaining Active Patterns
let (|Chain|_|) ap1 ap2 =
    let innerFn txt =
        ap1 txt 
        |> Option.bind(
            fun ((value:string),(remain:string))->
                ap2 remain 
                |> Option.bind (
                    fun (subsValue,subsRemain) ->
                        Some (value+subsValue,subsRemain)
                   )
            )
    innerFn
    
let (.>>.) = (|Chain|_|)

let (|A|_|) txt =
    let (|Ap|_|) = (|Scheme|_|) .>>. (|Colon|_|) .>>. (|BackslashEscape|_|)
    match txt with
    | Ap (value,remain) -> (value,remain) |> Some
    | _ -> None
*)

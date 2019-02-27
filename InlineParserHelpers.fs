module InlineParserHelpers


open System.Text.RegularExpressions
open InlineTypes


// Tokenizer helpers
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

   
// Converts a token to equivalent string
let (|Token2String|) token = 
    let translation = [ SBracketO,"["   ;       SBracketC,"]";      RBracketO,"(";     
                        RBracketC,")";          QuoteMark,"\"";     Whitespace," ";
                        Newline, "\\n";         Exclamation,"!";    Asterisk,"*";       
                        EmpOpenAst,"*(";        EmpCloseAst,")*";   EmpOpenUnd,"_(";    
                        EmpCloseUnd,")_";       Underscore,"_";     LessThan,"<";
                        MoreThan,">";           Backslash,"\\";
                        StrongOpenAst ,"**(";   StrongCloseAst,")**"
                        StrongOpenUnd ,"__(";   StrongCloseUnd,")__" ] |> Map.ofList
    match token with
    | Literal s -> s
    | Escaped s -> s
    | BacktickStr n -> "`" |> String.replicate n
    | tok             -> 
        match (Map.tryFind tok translation)  with
        | Some s -> s
        | None   -> failwithf "What? Trying to convert token with no translation to string"

let token2String = (|Token2String|)

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



// General Parsing Helpers
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
    | Some s,Some e when s=0 -> splitTokens tokens s e 
                                |> fun (a,b,c) -> (b,c)
                                |> Some
    | _ -> None

let (|TryParseUntil|_|) token tokens = 
    let sIdx   = findTokenIdx token tokens
    match sIdx with
    | Some s -> 
        let (before,after) = tokens |> List.splitAt s 
        let after2 = after |> (function t::rlst -> rlst |[]->[])
        (before,after2) |> Some
    | _ -> None

// Link and Image Parser Helpers
let (|ParseTextLink|_|) tokens = tokens |> (|TryParseWith|_|) SBracketO SBracketC

let (|ParseTextImg|_|) tokens = 
    match tokens with
    | TryParseUntil Exclamation (before,ParseTextLink(before2,inner,after))
        -> Some (before @ before2,
                 inner,
                 after)
    | _ -> None

let rec (|ParseDest|_|) tokens = 
    match tokens with
    | TryParseWithPrefix RBracketO RBracketC (inner,after)
        -> let maybeLink = inner |> stripWSHead |> stripWSTail
           maybeLink 
           |> List.tryFind(fun x-> x=Whitespace)
           |> function 
                // Don't want whitespace, only want continuous text
               | None   -> Some (maybeLink,after)
               | Some _ -> None
    | _ -> None

let (|ParseTitle|_|) tokens = tokens |> (|TryParseWithPrefix|_|) RBracketO RBracketC
let (|ParseRef|_|)   tokens = tokens |> (|TryParseWithPrefix|_|) SBracketO SBracketC

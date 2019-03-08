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


// Converts a token to equivalent string representation
let (|Token2String|) token = 
    let translation = [ SBracketO,"["   ;       SBracketC,"]";      RBracketO,"(";     
                        RBracketC,")";          QuoteMark,"\"";     Whitespace," ";
                        Newline, "\\n";         Exclamation,"!";    Asterisk,"*";       
                        EmpOpenAst,"*(";        EmpCloseAst,")*";   EmpOpenUnd,"_(";    
                        EmpCloseUnd,")_";       Underscore,"_";     LessThan,"<";
                        MoreThan,">";           Backslash,"\\";
                        StrongOpenAst ,"**(";   StrongCloseAst,")**"
                        StrongOpenUnd ,"__(";   StrongCloseUnd,")__" ] 
                       |> Map.ofList
    match token with
    | Literal s -> s
    | Escaped s -> s
    | BacktickStr n -> "`" |> String.replicate n
    | tok             -> 
        match (Map.tryFind tok translation)  with
        | Some s -> s
        | None   -> failwithf "What? Trying to convert token with no translation to string"

let token2String = (|Token2String|)

let tokenList2String tokens = tokens
                              |> List.map token2String
                              |> List.reduce (+)                   

let stripWSHead tokens = 
    tokens |> List.skipWhile (fun t -> t=Whitespace)

let stripWSTail tokens = 
           tokens 
           |> List.rev 
           |> stripWSHead 
           |> List.rev

// General Parsing Helpers
let (|FindTokenIdx|_|) tokenToFind tokens = 
    tokens |> List.tryFindIndex (fun t -> t = tokenToFind)

let findTokenIdx = (|FindTokenIdx|_|)

// Determines the index at which
// open and close delimiters are balanced
// starting from a given start index
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

// Split tokens into before,inner,after
// based on the start and relative end index of inner's Delimieters
let splitTokens tokens s e = 
    let (before,after1) = tokens |> List.splitAt s
    let (inner,after2)  = after1  |> List.splitAt (e+1)
    let inner2 = inner  |> (function | t::rlst -> rlst |[]->[])
    let after3 = after2 |> (function | t::rlst -> rlst |[]->[])
    (before,inner2,after3)

// Unwraps Styled InlineElement
let styledToInlineElement tokens =
    tokens |> List.map (function | Styled a -> a| a -> failwithf "What? Should only be list of styled Tokens, %A" a)

// Tries to extract the inner componenet between
// two balanced delimiters
// Returns the (before,inner,after) option     
let (|TryParseWith|_|) openDelim closeDelim tokens = 
    let sIdx   = findTokenIdx openDelim tokens
    let eIdx i = balancedCloseIdx openDelim closeDelim tokens i
    match (sIdx,Option.bind eIdx sIdx)  with
    | Some s,Some e -> splitTokens tokens s e |> Some
    | _ -> None

// openDelim must be the first token in tokens
let (|TryParseWithPrefix|_|) openDelim closeDelim tokens = 
    let sIdx   = findTokenIdx openDelim tokens
    let eIdx i = balancedCloseIdx openDelim closeDelim tokens i
    match (sIdx,Option.bind eIdx sIdx)  with
    | Some s,Some e when s=0 -> splitTokens tokens s e 
                                |> fun (b,i,a) -> (i,a) // keep only inner and after 
                                |> Some                 // before is only the delim
    | _ -> None
    
let (|TryParseUntil|_|) token tokens = 
    let sIdx   = findTokenIdx token tokens
    match sIdx with
    | Some s -> 
        let (before,after) = tokens |> List.splitAt s 
        let after2 = after |> (function t::rlst -> rlst |[]->[])
        (before,after2) |> Some
    | _ -> None

// Called to parse remainder of tokens if 
// original match failed to form a valid style
let parseRest openDelim styleParser tokens =
    match tokens with 
    | FindTokenIdx openDelim idx 
        ->tokens 
          |> List.splitAt (idx+1) 
          |> (fun (before,after)-> before @ (styleParser after))
    | _ -> tokens
    

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

let (|ParseRef|_|)   tokens = 
    match tokens with
    | TryParseWithPrefix SBracketO SBracketC (reference,after) ->
        match reference with
        | [] -> (None, after) |> Some  //ref may be optionally omitted
        | refr 
            -> refr 
               |> tokenList2String
               |> Some
               |> fun refrOpt -> (refrOpt,after) 
               |> Some
    | _ ->  None


// Configuration for Link or Image Parsers for parseLinksOrImg
let linkConfig  = ((|ParseTextLink|_|),Link )
let imageConfig = ((|ParseTextImg|_|) ,Image)

// Configuration for Link Reference or Image Reference Parsers for parseLinksOrImgRef
let linkRefConfig  = ((|ParseTextLink|_|),LinkRef )
let imageRefConfig = ((|ParseTextImg|_|) ,ImageRef)


//Configuration for Emphasis and Strong Parsers
let strongConfig = ([(StrongOpenAst,StrongCloseAst);(StrongOpenUnd,StrongCloseUnd)],Strong)
let empConfig    = ([(EmpOpenAst,EmpCloseAst);(EmpOpenUnd,EmpCloseUnd)],Emphasis)

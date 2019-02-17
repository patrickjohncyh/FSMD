﻿open System.Text.RegularExpressions

let (|RegexPrefix|_|) pat txt =
    // Match from start, ignore whitespace
    let m = Regex.Match(txt, "^[\\s]*" + pat + "[\\s]*") 
    match m.Success with
    | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
    | false -> None

let matchRegex = (|RegexPrefix|_|)

let (|BackslashEscape|_|) txt =
    let escapePattern = "\\\[\\!-\\/\\:-\\@\\[-\\`\\{\\~]"
    matchRegex escapePattern txt

// To Add : leading and trailing spaces and line endings removed,
//           and whitespace collapsed to single spaces.
let (|CodeSpan|_|) txt = 
    let codeSpanPattern = "([`]+)([^`]+)\1([^`]|$)+"
    matchRegex codeSpanPattern txt


//A link contains
    // link text (the visible text), [inlineElement]
    // a link destination (the URI that is the link destination), (URI "title"), URI seems like plain text
    // an optionally a link title -> Plain Text

// There are two basic kinds of links in Markdown. 
    //In inline links the destination and title are given immediately after the link text. 
    //In reference links the destination and title are defined elsewhere in the document.
let (|Link|_|) txt = 
    None

// Auto Link :  "< Absolute URI >"
// Absolute URI : "Scheme : ASCII (Exlcuding Ws < >)"   
let (|AutoLink|_|) txt = 
    let scheme = "[A-Za-z][A-Za-z0-9\+\-\.]{1,31}"
    let uri    = "[^<>\\s]*"
    let absoluteUri = scheme + ":" + uri
    let uriPattern    = "<" + absoluteUri + ">"
    matchRegex uriPattern txt    


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
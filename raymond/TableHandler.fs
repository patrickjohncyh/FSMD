module TableHandler

open System.Text.RegularExpressions
open TableTypes

let (|Regex|_|) pat txt =                                                   //detects pattern(at first char in string) and returns remaining string. Detected pattern value not stored. 
    let m = Regex.Match(txt,"^"+ pat)                                       //always checks first char onwards of string, not anywhere else 
    match m.Success with
    | true -> (txt.Substring(m.Value.Length)) |> Some
    | false -> None

let (|RegexExtract|_|) pat txt =                                            //extracts only relevant data from a cell             
    let m = Regex.Match(txt,pat)               
    match m.Success with
    | true -> (m.Value) |> Some
    | false -> None

let (|RegexSplitCellFromTable|_|) pat txt =                                  //detects entire cells and returns (singleCell, remainingRawInput)
    let m = Regex.Match(txt, pat)
    match m.Success with
    | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
    | false -> None

let rec tableTokeniser tableStr =                                           //input: entire table string, each row separated by /n. output: a list of tabletokens(rep. the entire table) containing Cellcontents, RowEnd, DelimRow
    match tableStr with
    | Regex "\|[\s]*$" remainingRawInput -> []                              //end of table 
    | _  ->
        let (matchedToken, remainingRawInput) = 
            match tableStr with
            | Regex "\| *\n *" remainingRawInput -> (NewRow,remainingRawInput)                                                          // To end the row "|abc", where a,c = 0 or more whitespace, b = newline 
            | Regex "\| +\-{3,} *" remainingRawInput -> (DelimCell,remainingRawInput)                                                   // To match delimiter rows |nx where n=at least 1 space, x = at least 3 "-"
            | RegexSplitCellFromTable "^\| +((\\\\\|)|[^\|])*" (singleCell,remainingRawInput) -> (Cell singleCell,remainingRawInput)    // To match |nx where n=at least 1 space, x= any character or "\|"  . 
            | _ -> failwithf "Does not contain table format."

        match (tableTokeniser remainingRawInput) with
        | [] -> [matchedToken]
        | tokenList -> matchedToken :: tokenList

let inlineParser (inputString : string) : InlineElement List =                      //stub test function. Written by another teammate.
    [Text inputString] 

let tableParser (lst : TableTokens List) : (InlineElement List List * InlineElement List List List) = 
    
    let (|FirstCellIndexOf|_|) (token:TableTokens) =                                //returns first index of the specified token 
        List.tryFindIndex (fun x -> x = token) 
    
    let extractCellContent = function                                               //converts Cell to string then extracts cell data, throwing away any formatting whitespace 
        | Cell str -> 
            match str with
            | RegexExtract "([^\|\s])([\s]*[\S]+)*" cellContent -> cellContent 
            | _ -> ""       //empty cell

        | _ -> failwithf "should only be Cell type! unknown type detected!"


    let rec parseCellContent = function                                             //pass to inLineParser to handle styles 
        | h :: t -> 
            (inlineParser h) :: parseCellContent t

        | [] -> []

    let rec splitIntoRows lst =                                                     //splits (token list) into (token list list) at every NewRow token
        match lst with 
        | FirstCellIndexOf NewRow index when (index > 0) ->
            lst.[0..(index - 1)] :: (splitIntoRows lst.[(index + 1)..])

        | FirstCellIndexOf NewRow index when (index = 0) ->
            match lst with 
            | h :: t -> splitIntoRows t
            | _ -> []                                                               // in case table ends with newline

        | _ -> [lst]

    let headerList =                                                                //token list->InlineElement List List
        match lst with
        | FirstCellIndexOf DelimCell index when (index > 0) ->
            lst.[0..(index-1)]
            |> List.filter (fun x -> x <> NewRow) 
            |> List.map extractCellContent
            |> parseCellContent 

        | FirstCellIndexOf DelimCell index when (index = 0) ->
                failwithf "table header missing"

        | _ -> failwithf "no delimiter row detected! table body missing"      

    let bodyList =                                                                  //token list -> InlineElement List List List
        match lst with
        | FirstCellIndexOf DelimCell index when (index > 0) ->
            lst.[(index + 1)..]
            |> List.filter (fun x -> x <> DelimCell) 
            |> splitIntoRows
            |> List.map (List.map extractCellContent)
            |> List.map parseCellContent

        | FirstCellIndexOf DelimCell index when (index = 0) ->
            failwithf "table header missing"

        | _ -> failwithf "no delimiter row detected! table body missing"     

    (headerList, bodyList)


  //Top level function
let tableHandler str =
    let tokenisedTableList = tableTokeniser str
    let (headerList, bodyListList) =
        tableParser tokenisedTableList
    Table {headerRow = headerList ; bodyRows = bodyListList}
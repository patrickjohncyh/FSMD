open System.Text.RegularExpressions

type Block =
    | Table of TableCells    //put into generic block handler
//  | NumberList               //List

and TableCells = {
    headerRow : InlineElement List List;
    bodyRows : InlineElement List List List
}

and InlineElement = 
    | Link    
    | StrongBox
    | Emphasis
    | Plain     

type TableTokens = 
    | RowEnd   |DelimRow    |EndTable                    // | CssOpen    |CssClose    | Newline    | Whitespace  
    | Cell of string
    | CellContent of string  

let (|Regex|_|) pat txt =                   //detects pattern(at first char in string) and returns remaining string. Detected pattern value not stored. 
    let m = Regex.Match(txt,"^"+ pat)       //^ checked: always checks first char onwards of string, not anywhere else 
    match m.Success with
    | true -> (txt.Substring(m.Value.Length)) |> Some
    | false -> None

let (|RegexExtract|_|) pat txt =            //extracts relevant data from a cell               
    let m = Regex.Match(txt,"^"+ pat)               
    match m.Success with
    | true -> (m.Value) |> Some
    | false -> None

let (|RegexKeepContent|_|) pat txt =        //detects pattern and returns (token value, remainingRawInput)
    let m = Regex.Match(txt, pat)
    match m.Success with
    | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
    | false -> None

let extractCellContent cell =
    match cell with
    | RegexExtract "([^\|\s])([\s]*[\S]+)*" cellContent -> CellContent cellContent 

let rec tableTokeniser txt =        //input: entire table string, each row separated by /n. output: a list of tabletokens(rep. the entire table) containing Cellcontents, RowEnd, DelimRow
    match txt with
    | "|" -> [EndTable]
    | _  ->
        let (matchedToken, remainingRawInput) = 
            match txt with
            | Regex "\|\n" remainingRawInput -> (RowEnd,remainingRawInput)                      //checked. To end the row "|\n". NOTE: final RowEnd issue
            | Regex "\| +\-{3,} *" remainingRawInput -> (DelimRow,remainingRawInput)            //checked. To match delimiter rows |nx where n=at least 1 space, x = at least 3 "-"
            | RegexKeepContent "^\| +((\\\\\|)|[^\|])*" (cell,remainingRawInput) -> 
                    (CellContent (extractCellContent cell),remainingRawInput)                     //checked. To match |nx where n=at least 1 space, x= any character or "\|"  
            | _ -> failwithf "Does not contain table format."

        match (tableTokeniser remainingRawInput) with
        | [] -> [matchedToken]
        | tokenList -> matchedToken :: tokenList

  





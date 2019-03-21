module BlockHandlers

open System.Text.RegularExpressions
open Types
open InlineParser
open BlockParser

let h1BlockHandle (inputString:string) =
    [inputString |> inlineParser |> H1]

let h2BlockHandle (inputString:string) =
    [inputString |> inlineParser |> H2]

let h3BlockHandle (inputString:string) =
    [inputString |> inlineParser |> H3]

let h4BlockHandle (inputString:string) =
    [inputString |> inlineParser |> H4]

let h5BlockHandle (inputString:string) =
    [inputString |> inlineParser |> H5]

let h6BlockHandle (inputString:string) =
    [inputString |> inlineParser |> H6]

let paragraphBlockHandler (inputString:string):Block list =
    [inputString |> inlineParser |> Paragraph]

let codeBlockHandler (inputString:string):Block list =
    [ [inputString |> Text] |> CodeBlock ]

let blockQuoteHandler blockDispatcher (inputString:string) :Block list=
    let inner = [inputString]
                |> Some 
                |> blockParser
                |> blockDispatcher
    [inner |> QuoteBlock ]

let listBlockHandler blockDispatcher (inputString: string) =
    let rec checkAppend (outputList: string list) (stringList:string list) =                //appends next line if not new list element
        let rec needAppend (inputString: string):bool =                                                 //lines without labels are appended to prev line
            let identifyLabel (inputString:string):bool =                                               //function will crash if 1st line does not have a label
                let correctLabel (inputString: string): bool =
                    let strContainsOnlyNumber (str:string) = System.Int32.TryParse str |> fst
                    let strContainsOnlyAlphabet(str:string) = 
                        str
                        |> Seq.tryFind (fun c -> not(System.Char.IsLetter(c)))
                        |> Option.isNone
                    (strContainsOnlyNumber inputString) || (strContainsOnlyAlphabet inputString)
                match inputString with
                |a when a.[a.Length - 1] = '.' -> correctLabel a.[0..(a.Length - 2)]
                |a when a.[a.Length - 1] = ')' -> correctLabel a.[0..(a.Length - 2)] 
                |"*" -> true
                |"+" -> true
                |"-" -> true
                |_ -> false

            match inputString with
            |"" -> failwithf ("empty line detected")
            |a when a.[0] = ' ' -> needAppend a.[1..]
            |a -> 
                let spaceIndex = a.IndexOf " "
                if spaceIndex = -1 then not(identifyLabel a)
                else not(identifyLabel a.[0..(spaceIndex - 1)])

        match stringList with
        |[] -> outputList
        |a when not(needAppend a.[0]) -> checkAppend (List.append outputList [a.[0]]) a.[1..]
        |a -> checkAppend (List.append outputList.[0..(outputList.Length - 2)] [outputList.[outputList.Length - 1] + " " + a.[0]]) a.[1..]

    let getTuple (inputString: string): string*string =
        let rec notSpaceIndex (inputString:string) (index:int) =
            match inputString with
            |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
            |a -> 
                if a.IndexOf " " = -1 then -1
                else a.IndexOf " " + index

        if (notSpaceIndex inputString 0) = -1 then (inputString, "")
        else (inputString.[0..((notSpaceIndex inputString 0) - 1)], inputString.[((notSpaceIndex inputString 0) + 1)..])

    let compareIndent (a,b) =                                                                                           //handle 1st indent works with this function
        if b > a then (a+1)
        else b

    let getIndentLevel (inputString: string):int =                                                                      //get proper indent levels, ignoring useless spaces
        let rec notSpaceIndex (inputString:string) (index:int) =
            match inputString with
            |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
            |_ -> index
        (notSpaceIndex inputString 0)/3

    let appendHead (intList: int list): int list = intList.[0] :: intList

    let handleFirstIndent (intList: int list):int list =                                                                //special case: 1st row is indented
        let removeNegative (inputInt: int):int =                                                
            if inputInt >= 0 then inputInt
            elif inputInt = -1 then 0
            else failwithf ("Indent level is < -1")
        if intList.[0] = 1 then List.map removeNegative (List.map (fun x -> x - 1) intList)
        elif intList.[0] = 0 then intList
        else failwithf ("First indent is not 1 or 0")                                                                   //note: 1st row can only be indented at most 1 time

    let removeIndent (inputString: string) =                                                                            //strip all indents from label
        let rec notSpaceIndex (inputString:string) (index:int) =
             match inputString with
             |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
             |_ -> index
        let indentNum = (notSpaceIndex inputString 0)
        inputString.[indentNum..]

    let removeHead (intList: int list):int list = intList.[1..]

    let properIndent (intList: int list) (stringList: string list):string list =                                        //properly indent the label according to prev calculated indents
        let tupleList = List.zip intList stringList
        let addIndent (inputTuple:int*string):string =
            let indentString = [0..(fst inputTuple)] |> removeHead |> List.map (fun _ -> "   ") |> String.concat ""
            indentString + snd inputTuple
        tupleList |> List.map addIndent

    // Fix for Fable not supporting List.pariwise
    let pairWise x = x 
                    |>Seq.pairwise 
                    |> Seq.toList  

    let individualString = inputString.Split '\n' |> Array.toList |> checkAppend [] |> List.map getTuple
    let indentLevel = individualString 
                      |> List.map fst 
                      |> List.map getIndentLevel 
                      |> appendHead |> pairWise
                      |> List.map compareIndent 
                      |> handleFirstIndent

    let noIndent = individualString |> List.map fst |> List.map removeIndent
    let perfectList = List.zip (properIndent indentLevel noIndent) (List.map snd individualString) |> List.zip indentLevel

    let getListType (inputString: string): ListType =
        let identifier = inputString.[inputString.Length - 1]
        match identifier with
        |'.' -> DotList
        |'*' -> StarList
        |')' -> BracketList
        |'+' -> PlusList
        |'-' -> MinusList
        |_ -> failwithf ("perfectList is not perfect")

    let rec separateList (inputList: (int*(string*string))list) =                                                           //separate a list chunk into separate lists
        let rec getListTypeList (mainType: ListType) outputList (theRestList:(int*(string*string))list) (index:int) =
            match theRestList with
            |[] -> outputList
            |a -> getListTypeList (mainType: ListType) (List.append outputList [(a.[0] |> snd |> fst |> getListType)]) a.[1..] (index:int)

        let compareListType a = fst a = snd a
        

        let f tuple = 
            if (fst tuple) = true then []
            else [snd tuple]

        let lastPlusOne (inputList: int list) = List.append inputList.[0..inputList.Length - 2] [inputList.[inputList.Length - 1] + 1]
        let sndMinusOne a = ((fst a), ((snd a) - 1))

        let placeHolder = inputList |> List.map snd |> List.map fst |> List.map getListType |>pairWise |> List.map compareListType
        let placeHolder2 = List.zip placeHolder [1..placeHolder.Length] |> List.collect f |> List.append [0]
        let placeHolder3 = 
            if placeHolder2.[placeHolder2.Length - 1] = (inputList.Length) then placeHolder2 |> lastPlusOne |>pairWise|> List.map sndMinusOne
            else List.append placeHolder2 [inputList.Length - 1] |> lastPlusOne |> pairWise |> List.map sndMinusOne

        let finalSeparate (inputList: (int*(string*string))list) intTuple = inputList.[(fst intTuple)..(snd intTuple)]

        placeHolder3 |> List.map (finalSeparate perfectList)

    let listList = perfectList |> separateList

    let equalZero inputInt =
        if inputInt = 0 then true
        else false

    let rec reduceZero (indentLines:(int * (string * string)) list) (tupleList:(int * (string * string)) list)  = //takes in tuple list, breaks down into 2 tuplelist: sublist followed by top level list
        match tupleList with 
        |[] -> (indentLines, tupleList) 
        |a when not(equalZero (fst a.[0])) -> reduceZero (List.append indentLines [a.[0]]) tupleList.[1..]
        |_ -> (indentLines, tupleList)

    let getMarkDown (tupleList:(int * (string * string)) list):string =                                           //if a sublist is found, change into a top level list and pass to blockparser
        tupleList |> List.map snd |> List.map (fun (a,b) -> a + " " + b) |> List.map (fun x -> x.[3..]) |> String.concat "\n"

    let getListHead (input:(int * (string * string)) list) = input.[0]

    let listList = separateList perfectList

    let convertHTMLFriendly (inputString: string): string =
        let identifier = inputString.[inputString.Length - 1]
        match identifier with
        |'*' -> "disc"
        |'+' -> "disc"
        |'-' -> "disc"
        |'.' -> inputString.[0..(inputString.Length - 2)]
        |')' -> inputString.[0..(inputString.Length - 2)]
        |_ -> failwithf ("perfectList is not perfect")

    let headerList = listList |> List.map getListHead |> List.map snd |> List.map fst |> List.map convertHTMLFriendly

    let rec listToDOMElement (output: ListStructure list) (tupleList:(int * (string * string)) list): ListStructure list =  //convert parsed list to meta DOM
        match tupleList with 
        |[] -> output
        |a when equalZero (fst a.[0]) -> 
            let newOutput = List.append output [a.[0] |> snd |> snd |> fun x -> [x] |> Some |> blockParser |> blockDispatcher |> ListLines ]
            listToDOMElement newOutput a.[1..]
        |a -> 
            let list1 = fst (reduceZero [] a)
            let newOutput = List.append output [ list1 |> getMarkDown |> fun x -> [x] |> Some |> blockParser |> blockDispatcher |> InnerList ]
            let newTupleList = snd (reduceZero [] a)
            listToDOMElement newOutput newTupleList 

    let structureList = List.map (listToDOMElement []) listList
    let getBlock (input: ListStructure list * string):Block = ListBlock(input)
    List.map getBlock (List.zip structureList headerList)
    
module BlockHandler

open System.Text.RegularExpressions
open Types

let h1BlockHandle (inputString:string) =
    [H1(inLineParser (inputString.[2..]))]

let h2BlockHandle (inputString:string) =
    [H2(inLineParser (inputString.[3..]))]

let h3BlockHandle (inputString:string) =
    [H3(inLineParser (inputString.[4..]))]

let h4BlockHandle (inputString:string) =
    [H4(inLineParser (inputString.[5..]))]

let h5BlockHandle (inputString:string) =
    [H5(inLineParser (inputString.[6..]))]

let h6BlockHandle (inputString:string) =
    [H6(inLineParser (inputString.[7..]))]

let paragraphBlockHandler (inputString:string):Block list =
    let parsedString = inputString.Replace("\n", " ")
    [Paragraph(inLineParser parsedString)]

let codeBlockHandler (inputString:string):Block list =
    let separatedString = inputString.Split "\n" |> Array.toList
    let removeIndent (input:string):string = input.[4..]
    separatedString |> List.map removeIndent |> String.concat "\n" |> blockParser

let blockQuoteHandler (inputString:string):Block list=
    let strContainsOnlyNumber (s:string) = System.Int32.TryParse s |> fst
    let removeIndent (inputString: string) =                                                                            //strip all indents from label
        let rec notSpaceIndex (inputString:string) (index:int) =
            match inputString with
            |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
            |_ -> index
        let indentNum = (notSpaceIndex inputString 0)
        inputString.[indentNum..]
    let checkForArrow (inputString: string): bool = (inputString.IndexOf "> ") = 0

    let rec checkAppend (outputList: string list) (stringList:string list): string list =  //check if next line is suppose to be appended to the prev line
        match stringList with
        |[] -> outputList
        |a when a.[0].IndexOf "> " = 0 -> checkAppend (List.append outputList [a.[0]]) a.[1..]
        |a when a.[0].IndexOf "> " = -1 -> checkAppend (List.append outputList.[0..(outputList.Length - 2)] [outputList.[outputList.Length - 1] + " " + a.[0]]) a.[1..]
        |a -> 
            let indentRemoved = removeIndent a.[0]
            if (indentRemoved.IndexOf "> " = 0) then checkAppend (List.append outputList [indentRemoved]) a.[1..]
            else checkAppend (List.append outputList.[0..(outputList.Length - 2)] [outputList.[outputList.Length - 1] + " " + a.[0]]) a.[1..]

    let separatedString = inputString.Split "\n" |> Array.toList |> checkAppend []
    let removeArrow (input:string):string = input.[2..]
    separatedString |> List.map removeArrow |> String.concat "\n" |> blockParser

let blockQuoteHandlerTest (inputString:string)=
    let strContainsOnlyNumber (s:string) = System.Int32.TryParse s |> fst
    let removeIndent (inputString: string) =                                                                            //strip all indents from label
        let rec notSpaceIndex (inputString:string) (index:int) =
            match inputString with
            |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
            |_ -> index
        let indentNum = (notSpaceIndex inputString 0)
        inputString.[indentNum..]
    let checkForArrow (inputString: string): bool = (inputString.IndexOf "> ") = 0

    let rec checkAppend (outputList: string list) (stringList:string list): string list =  //check if next line is suppose to be appended to the prev line
        match stringList with
        |[] -> outputList
        |a when a.[0].IndexOf "> " = 0 -> checkAppend (List.append outputList [a.[0]]) a.[1..]
        |a when a.[0].IndexOf "> " = -1 -> checkAppend (List.append outputList.[0..(outputList.Length - 2)] [outputList.[outputList.Length - 1] + " " + a.[0]]) a.[1..]
        |a -> 
            let indentRemoved = removeIndent a.[0]
            if (indentRemoved.IndexOf "> " = 0) then checkAppend (List.append outputList [indentRemoved]) a.[1..]
            else checkAppend (List.append outputList.[0..(outputList.Length - 2)] [outputList.[outputList.Length - 1] + " " + a.[0]]) a.[1..]

    let separatedString = inputString.Split "\n" |> Array.toList |> checkAppend []
    let removeArrow (input:string):string = input.[2..]
    separatedString |> List.map removeArrow |> String.concat "\n"

let listBlockHandler (inputString: string) =
    let rec checkAppend (outputList: string list) (stringList:string list) =                //appends next line if not new list element
        let rec needAppend (inputString: string):bool =                                                 //lines without labels are appended to prev line
            let identifyLabel (inputString:string):bool =                                               //function will crash if 1st line does not have a label
                let strContainsOnlyNumber (s:string) = System.Int32.TryParse s |> fst       
                match inputString with
                |a when a.[a.Length - 1] = '.' -> strContainsOnlyNumber a.[0..(a.Length - 2)]
                |a when a.[a.Length - 1] = ')' -> strContainsOnlyNumber a.[0..(a.Length - 2)] 
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

    let individualString = inputString.Split "\n" |> Array.toList |> checkAppend [] |> List.map getTuple
    let indentLevel = individualString |> List.map fst |> List.map getIndentLevel |> appendHead |> List.pairwise |> List.map compareIndent |> handleFirstIndent
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

        let placeHolder = inputList |> List.map snd |> List.map fst |> List.map getListType |> List.pairwise |> List.map compareListType
        let placeHolder2 = List.zip placeHolder [1..placeHolder.Length] |> List.collect f |> List.append [0]
        let placeHolder3 = 
            if placeHolder2.[placeHolder2.Length - 1] = (inputList.Length) then placeHolder2 |> lastPlusOne |> List.pairwise |> List.map sndMinusOne
            else List.append placeHolder2 [inputList.Length - 1] |> lastPlusOne |> List.pairwise |> List.map sndMinusOne

        let finalSeparate (inputList: (int*(string*string))list) intTuple = inputList.[(fst intTuple)..(snd intTuple)]

        placeHolder3 |> List.map (finalSeparate perfectList)

    let listList = perfectList |> separateList

    let equalZero inputInt =
        if inputInt = 0 then true
        else false

    let rec reduceZero (indentLines:(int * (string * string)) list) (tupleList:(int * (string * string)) list)  = //takes in tuple list, breaks down into 2 tuplelist: sublist followed by top level list
        match tupleList with 
        |[] -> (indentLines, tupleList) 
        |a when equalZero (fst a.[0]) -> reduceZero (List.append indentLines [a.[0]]) tupleList.[1..]
        |_ -> (indentLines, tupleList)

    let getMarkDown (tupleList:(int * (string * string)) list):string =                                           //if a sublist is found, change into a top level list and pass to blockparser
        tupleList |> List.map snd |> List.map (fun (a,b) -> a + " " + b) |> List.map (fun x -> x.[3..]) |> String.concat "\n"

    let getListHead (input:(int * (string * string)) list) = input.[0]

    let listList = separateList perfectList
    let headerList = listList |> List.map getListHead |> List.map snd |> List.map fst

    let rec listToDOMElement (output: ListStructure list) (tupleList:(int * (string * string)) list): ListStructure list =  //convert parsed list to meta DOM
        match tupleList with 
        |[] -> output
        |a when equalZero (fst a.[0]) -> 
            let newOutput = List.append output [ListLines(a.[0] |> snd |> snd |> blockParser)]
            listToDOMElement newOutput a.[1..]
        |a -> 
            let list1 = fst (reduceZero [] a)
            let newOutput = List.append output [InnerList(list1 |> getMarkDown |> blockParser)]
            let newTupleList = snd (reduceZero [] a)
            listToDOMElement newOutput newTupleList 

    let structureList = List.map (listToDOMElement []) listList
    let getBlock (input: ListStructure list * string):Block = ListBlock(input)
    List.map getBlock (List.zip structureList headerList)

//This is a partial duplicate of listBlockHandler which is used to check the parsing
let listBlockHandlerTest (inputString: string) =
    let rec checkAppend (outputList: string list) (stringList:string list) =  //appends next line if not new list element
        let rec needAppend (inputString: string):bool =
            let identifyLabel (inputString:string):bool =
                let strContainsOnlyNumber (s:string) = System.Int32.TryParse s |> fst  // do for )
                match inputString with
                |a when a.[a.Length - 1] = '.' -> strContainsOnlyNumber a.[0..(a.Length - 2)]
                |a when a.[a.Length - 1] = ')' -> strContainsOnlyNumber a.[0..(a.Length - 2)]  
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

    let compareIndent (a,b) = 
        if b > a then (a+1)
        else b

    let handleFirstIndent (intList: int list):int list =
        let removeNegative (inputInt: int):int =
            if inputInt >= 0 then inputInt
            elif inputInt = -1 then 0
            else failwithf ("Indent level is < -1")
        if intList.[0] = 1 then List.map removeNegative (List.map (fun x -> x - 1) intList)
        elif intList.[0] = 0 then intList
        else failwithf ("First indent is not 1 or 0")

    let getIndentLevel (inputString: string):int =
        let rec notSpaceIndex (inputString:string) (index:int) =
            match inputString with
            |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
            |_ -> index
        (notSpaceIndex inputString 0)/3

    let appendHead (intList: int list): int list = intList.[0] :: intList

    let removeIndent (inputString: string) =
        let rec notSpaceIndex (inputString:string) (index:int) =
             match inputString with
             |a when a.[0] = ' ' -> notSpaceIndex a.[1..] (index + 1)
             |_ -> index
        let indentNum = (notSpaceIndex inputString 0)
        inputString.[indentNum..]

    let removeHead (intList: int list):int list = intList.[1..]

    let properIndent (intList: int list) (stringList: string list):string list =
        let tupleList = List.zip intList stringList
        let addIndent (inputTuple:int*string):string =
            let indentString = [0..(fst inputTuple)] |> removeHead |> List.map (fun _ -> "   ") |> String.concat ""
            indentString + snd inputTuple
        tupleList |> List.map addIndent

    let individualString = inputString.Split "\n" |> Array.toList |> checkAppend [] |> List.map getTuple
    let indentLevel = individualString |> List.map fst |> List.map getIndentLevel |> appendHead |> List.pairwise |> List.map compareIndent |> handleFirstIndent
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

    let rec separateList (inputList: (int*(string*string))list) =
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

        let placeHolder = inputList |> List.map snd |> List.map fst |> List.map getListType |> List.pairwise |> List.map compareListType
        let placeHolder2 = List.zip placeHolder [1..placeHolder.Length] |> List.collect f |> List.append [0]
        let listGroups = 
            if placeHolder2.[placeHolder2.Length - 1] = (inputList.Length) then placeHolder2 |> lastPlusOne |> List.pairwise |> List.map sndMinusOne
            else List.append placeHolder2 [inputList.Length - 1] |> lastPlusOne |> List.pairwise |> List.map sndMinusOne

        let finalSeparate (inputList: (int*(string*string))list) intTuple = inputList.[(fst intTuple)..(snd intTuple)]

        listGroups |> List.map (finalSeparate perfectList)

    perfectList |> separateList

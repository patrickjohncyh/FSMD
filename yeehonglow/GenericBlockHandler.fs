open System.Text.RegularExpressions
let countMatches wordToMatch (input : string) =
    Regex.Matches(input, Regex.Escape wordToMatch).Count

type InLine =
    |Italic
    |Plain 
    |Bold

type ListStructure =
    |JustList of ListStructure List
    |JustInLine of InLine
    |TupleStuff of InLine * ListStructure List

type Block =
    |Paragraph of InLine List
    |H1 of InLine List
    |H2 of InLine List
    |H3 of InLine List
    |H4 of InLine List
    |H5 of InLine List
    |H6 of InLine List
    |NumberList of ListStructure
    
let inLineParser (a:string): InLine List=
    failwithf("Not Implemented")

let h1BlockHandle (inputString:string) =
    H1(inLineParser (inputString.[2..]))

let h2BlockHandle (inputString:string) =
    H2(inLineParser (inputString.[3..]))

let h3BlockHandle (inputString:string) =
    H3(inLineParser (inputString.[4..]))

let h4BlockHandle (inputString:string) =
    H4(inLineParser (inputString.[5..]))

let h5BlockHandle (inputString:string) =
    H5(inLineParser (inputString.[6..]))

let h6BlockHandle (inputString:string) =
    H6(inLineParser (inputString.[7..]))

let paragraphBlockHandler inputString =
    Paragraph(inLineParser inputString)

let listBlockHandler (inputString:string) =
    let splitDot (aList: string) = aList.Split ' '

    let stickyIndent inputString =        //rewrite
        let indentNum = countMatches "/indent" inputString
        let intList = [1..indentNum]
        let indentList = List.map (fun _ -> "/indent") intList
        List.append indentList [inputString.[(indentNum*7)..(inputString.Length - 1)]]

    let rec handleIndent (stringList: string list) =
        match stringList with
        |"/indent"::t -> List.append ["/indent"] (handleIndent t)
        |lst -> [lst.[0]; String.concat " " lst.[1..]]

    let rec handleIndent (stringList: string list) =
        let tempList = []
        let indentNum = countMatches "/indent" stringList.[0]
        if indentNum = 0 then List.append tempList (stringList)
        else 

    let merge3More (stringList:string List) =  //fix this, take care of /indent/indent4.
        let stringLen = stringList.Length
        if stringLen > 2 then
            if stringList.[0] = "/indent" then handleIndent stringList
            else [stringList.[0]; String.concat " " stringList.[1..]]
        else stringList

    let removeSpace input =
        let rec removeFrontSpace (inputString:string) =
            match inputString.[0] with
            |' ' -> removeFrontSpace inputString.[1..]
            |a -> inputString

        let rec removeBackSpace (inputString:string) =
            let stringLen = inputString.Length
            match inputString.[stringLen - 1] with
            |' ' -> removeBackSpace inputString.[0..(stringLen - 2)]
            |a -> inputString
        removeFrontSpace (removeBackSpace input)

    let rec countIndent (input:int) (stringList: string List)=
        match stringList with 
        |[] -> input
        |"/indent"::t -> countIndent (input+1) t 
        |_::t -> countIndent (input) t

    let compareIndent (a,b) = 
        if b > a then (a+1)
        else b

    let individualList = ((inputString.Split "/n") |> Array.toList |> List.map removeSpace) |> List.map splitDot |> List.map Array.toList |> List.map merge3More
    let indentList = List.map (countIndent 0) individualList |> List.append [0] |> List.pairwise |> List.map compareIndent
    indentList

let a = listBlockHandler "1./n /indent 2. item two /n /indent /indent - sublist sublist"


type Name1 = 
    |Abc of int

let a:Name1 = Abc(1)





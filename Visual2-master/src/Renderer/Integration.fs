(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Integration
    Description: Code to integrate the emulator with the renderer
*)

/// integrate emulator code with renderer
module Integration


open FSMDTOP
open Types
open Views
open Refs
open Fable.Import.Browser
open Fable.Core.JsInterop

let HTMLToNode htmlElement:Node =htmlElement
let NodeToHTML node:HTMLElement = node

let rec elementToDOM element =
    let el = match element with
             | Text text      -> document.createTextNode text :> Node
             | Emphasis eList -> 
                let parent   = makeElement "em" "markdown-output" ""
                let children = eList |> List.map elementToDOM
                addToDOM parent children
             | Strong eList -> 
                let parent   = makeElement "strong" "markdown-output" ""
                let children = eList |> List.map elementToDOM
                addToDOM parent children   
             | CodeSpan eList -> 
                let parent   = makeElement "code" "markdown-output" ""
                let children = eList |> List.map elementToDOM
                addToDOM parent children
             | Hardbreak ->
                let br = document.createElement "br"
                HTMLToNode br
             | Softbreak -> 
                let space = document.createTextNode " " 
                HTMLToNode space
             | Link linkInfo ->
                let parent = document.createElement "a" :?> HTMLAnchorElement
                let linkText = List.map elementToDOM linkInfo.linkText
                let href =  
                    match linkInfo.linkDest with
                    | Text str :: _ -> str
                    | _ -> ""
                let title = 
                    match linkInfo.linkTitle with
                    | Some lst -> 
                        match lst with 
                        | Text str :: _ -> str
                        | _ -> ""
                    | None -> ""
                
                parent.title <- title 
                parent.href  <- href   
                addToDOM parent linkText
             | Image imageInfo ->
                let parent = document.createElement "img" :?> HTMLImageElement
                let alt =  match imageInfo.linkText with
                           | Text str :: _ -> str
                           | _ -> failwithf "link Title cannot have styles"
                let title = 
                    match imageInfo.linkTitle with
                    | Some lst -> 
                        match lst with 
                        | Text str :: _ -> str
                        | _ -> failwithf "link Title cannot have styles"
                    | None -> ""
                let src =  
                    match imageInfo.linkDest with
                    | Text str :: _ -> str
                    | _ -> failwithf "link Title cannot have styles"
                parent.alt <- alt 
                parent.src  <- src   
                parent.title <- title
                HTMLToNode parent
             | Katex str -> 
                    let parent = document.createElement "span"
                    let innerHTML = katex?renderToString(
                        str,
                        createObj [ 
                            "throwOnError" ==> false;
                            "displayMode"  ==> false])
                    parent.innerHTML <- innerHTML
                    parent.firstChild

             //| LinkRef lrInfo     ->     not implemented 
             //| ImageRef lrInfo    ->     not implemented
             | _ -> failwithf "What? Not implemented yet"
    el |> HTMLToNode

let blockListToDOM blockList =
    let parent = getHtml "viewer"
    let stubDiv = parent.firstChild
    let stubReplacer = document.createElement "div"
    let replaceStub (parent:Node) content =
        parent.replaceChild(content,stubDiv)
    let rec blockToDOM block = 
        match block with
        | Paragraph eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "p" "markdown-output" ""
               addToDOM parent children
        | H1 eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "h1" "markdown-output" ""
               addToDOM parent children
        | H2 eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "h2" "markdown-output" ""
               addToDOM parent children
        | H3 eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "h3" "markdown-output" ""
               addToDOM parent children
        | H4 eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "h4" "markdown-output" ""
               addToDOM parent children
        | H5 eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "h5" "markdown-output" ""
               addToDOM parent children
        | H6 eList ->
               let children = eList |> List.map elementToDOM
               let parent = makeElement "h6" "markdown-output" ""
               addToDOM parent children
        | CodeBlock eList ->                                            //cannot even produce blocklist
               let preTag =  makeElement "pre" "markdown-output" "" :> Node
               let codeTag = makeElement "code" "markdown-output" ""
               let children = eList |> List.map elementToDOM
               let parent = addToDOM codeTag children
               addToDOM preTag [parent]
        | QuoteBlock bList ->
               let children = bList |> List.map blockToDOM
               let parent = makeElement "blockquote" "markdown-output" ""
               addToDOM parent children
        | Table {headerRow = hRow; bodyRows = bRows} -> 
                let tableNode = document.createElement "table"
                let appendHeadRow (tableNode:Node) = 
                    let tr = document.createElement "tr"
                    let processHeadColumns (column : InlineElement List) =
                        let th = document.createElement "th"
                        let thContent = column |> List.map elementToDOM
                        addToDOM th thContent
                    hRow |> List.map processHeadColumns |> addToDOM tr |> tableNode.appendChild |> ignore
                    tableNode
                let appendBodyRows (tableNode:Node) =
                    let processOneBodyRow (entireRow:InlineElement List List) = 
                        let tr = document.createElement "tr"
                        let processBodyColumns (column : InlineElement List) =
                            let td = document.createElement "td"
                            let tdContent = column |> List.map elementToDOM
                            addToDOM td tdContent
                        entireRow |> List.map processBodyColumns |> addToDOM tr 
                    bRows |> List.map processOneBodyRow |> addToDOM tableNode |> ignore
                    tableNode
                tableNode |> appendHeadRow |> appendBodyRows
        | ListBlock (lstStrucLst, indexAttribute) -> 
                let parent = match indexAttribute with
                            | "disc" -> let ul = document.createElement "ul"
                                        ul.style.listStyleType <- "disc"
                                        ul
                            | _ -> let ol = document.createElement "ol" :?> HTMLOListElement
                                   ol.``type`` <- "1"
                                   ol.start <- float(indexAttribute)
                                   ol :> HTMLElement
                let rec innerFn lstStruclst (parent:Node) = 
                    match lstStruclst with
                    | ListLines line :: rlst 
                        -> let child = line.[0] |> blockToDOM 
                           let li = document.createElement "li"
                           li.appendChild child |> ignore
                           parent.appendChild li |> ignore
                           innerFn rlst parent
                    | InnerList innerList :: rlst
                        -> let child = innerList.[0] |> blockToDOM
                           let lastChild = parent.lastChild
                           lastChild |> (function | null -> parent.appendChild child
                                                  | _ -> lastChild.appendChild child) 
                                     |> ignore 
                           innerFn rlst parent 
                    | []-> parent
                innerFn lstStrucLst parent                
        | _ -> failwithf "What? Not implemented yet"

    blockList 
    |> List.map blockToDOM 
    |> List.map HTMLToNode 
    |> addToDOM stubReplacer
    |> replaceStub parent

let removeAfterDoneUsedForPrintTest (list:Block list) =
    printfn "%A" list
    list

let parseText lines =
    FSMDTop lines |> removeAfterDoneUsedForPrintTest |> blockListToDOM |> ignore
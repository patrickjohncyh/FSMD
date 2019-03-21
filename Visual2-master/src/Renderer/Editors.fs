(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Editors
    Description: Interface with Monaco editor buffers
*)

/// Interface with monaco editor buffers
module Editors

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open Refs
open Tooltips

open CommonData
open Memory

let editorOptions (readOnly : bool) =
    let vs = Refs.vSettings
    createObj [

                        // User defined settings
                        "theme" ==> vs.EditorTheme
                        "renderWhitespace" ==> vs.EditorRenderWhitespace
                        "fontSize" ==> vs.EditorFontSize
                        "wordWrap" ==> vs.EditorWordWrap

                        // Application defined settings
                        "value" ==> "";
                        "renderIndentGuides" ==> false
                        "fontFamily" ==> "fira-code"
                        "fontWeight" ==> "bold"
                        "language" ==> "markdown";
                        "roundedSelection" ==> false;
                        "scrollBeyondLastLine" ==> false;
                        "readOnly" ==> readOnly;
                        "automaticLayout" ==> true;
                        "minimap" ==> createObj [ "enabled" ==> false ];
                        "glyphMargin" ==> true
              ]


let updateEditor tId readOnly =
    if tId <> -1 then
        let eo = editorOptions readOnly
        Refs.editors.[tId]?updateOptions (eo) |> ignore

let setTheme theme =
    window?monaco?editor?setTheme (theme)


let updateAllEditors readOnly =
    Refs.editors
    |> Map.iter (fun tId _ -> if tId = Refs.currentFileTabId then readOnly else false
                              |> updateEditor tId)
    let theme = Refs.vSettings.EditorTheme
    Refs.setFilePaneBackground (
        match theme with
        | "one-light-pro" | "solarised-light" -> "white"
        | _ -> "black")
    setTheme (theme) |> ignore
    setCustomCSS "--editor-font-size" (sprintf "%spx" vSettings.EditorFontSize)

// Enable the editor once execution has completed
let enableEditors() =
    Refs.fileTabMenu.classList.remove ("disabled-click")
    Refs.fileTabMenu.onclick <- (fun _ -> createObj [])
    updateEditor Refs.currentFileTabId false
    Refs.darkenOverlay.classList.add ([| "invisible" |])


let mutable decorations : obj list = []
let mutable lineDecorations : obj list = []

[<Emit "new monaco.Range($0,$1,$2,$3)">]
let monacoRange _ _ _ _ = jsNative

[<Emit "$0.deltaDecorations($1, [
    { range: $2, options: $3},
  ]);">]
let lineDecoration _editor _decorations _range _name = jsNative

[<Emit "$0.deltaDecorations($1, [{ range: new monaco.Range(1,1,1,1), options : { } }]);">]
let removeDecorations _editor _decorations =
    jsNative
(*
// Remove all text decorations associated with an editor
let removeEditorDecorations tId =
    if tId <> -1 then
        List.iter (fun x -> removeDecorations Refs.editors.[tId] x) decorations
        decorations <- []

let editorLineDecorate editor number decoration (rangeOpt : (int * int) option) =
    let model = editor?getModel ()
    let lineWidth = model?getLineMaxColumn (number)
    let posStart = match rangeOpt with | None -> 1 | Some(n, _) -> n
    let posEnd = match rangeOpt with | None -> lineWidth | Some(_, n) -> n
    let newDecs = lineDecoration editor
                    decorations
                    (monacoRange number posStart number posEnd)
                    decoration
    decorations <- List.append decorations [ newDecs ]

// highlight a particular line
let highlightLine tId number className =
    editorLineDecorate
        Refs.editors.[tId]
        number
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> className
        ])
        None

let highlightGlyph tId number glyphClassName =
    editorLineDecorate
        Refs.editors.[tId]
        number
        (createObj [
            "isWholeLine" ==> true
            "glyphMarginClassName" ==> glyphClassName
        ])
        None

let highlightNextInstruction tId number =
    if number > 0 then highlightGlyph tId number "editor-glyph-margin-arrow"


//*************************************************************************************
//                              EDITOR CONTENT WIDGETS
//*************************************************************************************

type MemDirection = | MemRead | MemWrite

/// find editor Horizontal char position after end of code (ignoring comment)
let findCodeEnd (lineCol : int) =
    let tabSize = 6
    match Refs.currentTabText() with
    | None -> 0
    | Some text ->
        if text.Length <= lineCol then
            0
        else
            let line = text.[lineCol]
            match String.splitRemoveEmptyEntries [| ';' |] line |> Array.toList with
            | s :: _ -> (s.Length / tabSize) * tabSize + (if s.Length % tabSize > 0 then tabSize else 0)
            | [] -> 0
*)
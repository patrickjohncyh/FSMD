
/// Interface with monaco editor buffers
module Editors

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open Refs

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

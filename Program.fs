open System
open InlineParser

  [<EntryPoint>]
    let main argv =
        printfn "Hello World"
        inlineParser "Test" |> printfn "%A"
        0 // ret
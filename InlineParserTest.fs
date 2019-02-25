module InlineParserTest

open System
open Expecto
open FsCheck


let revOfRevIsOrig (x: int list) = 
    List.rev (List.rev x) = List.rev x

[<Tests>]
let linkTest =


    // Basic Tests

    // Link no title
    // [Test link](www.google.com)
 
    // Link with title
    // [Test link](www.google.com)(Title)

    // Link no text
    // [](www.google.com)

    // Link no dest
    // [Test link)()


    // Link with inner sq brackets

    // 

    testProperty "Simple Test" <| revOfRevIsOrig


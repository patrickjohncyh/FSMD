module InlineParserTest

open System
open Expecto
open FsCheck
open InlineParser


let revOfRevIsOrig (x: int list) = 
    List.rev (List.rev x) = List.rev x


let makeTestCase testFn testName (testInput,expOutput,msg) = 
    testCase testName <| fun () ->
        Expect.equal (testFn testInput)  expOutput msg

let testOfList groupName testFn testTripList = 
    testTripList 
    |> List.indexed
    |> List.map (fun (idx,triple) -> makeTestCase testFn (groupName+"_"+string(idx)) triple)
    |> testList groupName

    

[<Tests>]
let textTest =
    let tests = [
        (
        "",
        [],
        "Empty input"
        )

        (
        "Hello",
        [Text "Hello"],
        "Single Word"
        )

        ("Hello World this is some text with numbers 1 2 3 4",
         [Text "Hello World this is some text with numbers 1 2 3 4"],
         "Sentence")

        (
        "! @ # $ % ^ & * ( ) _ +",
        [Text "! @ # $ % ^ & * ( ) _ +"],
        "Symbols"
        )

        (
        "! @ # T]e[[st $ > \\ Rand()m > ~!! * ! @... <x<x a_b_c \"Quoted\" ", 
        [Text "! @ # T]e[[st $ > \\ Rand()m > ~!! * ! @... <x<x a_b_c \"Quoted\" "],
        "Symbols and Text"
        )
    ]

    ("Text Parsing",inlineParser,tests) |||> testOfList
    



[<Tests>]
let linkTestPositive =
    let tests = [
        (
        "[Test link](www.google.com)",
        [Link {linkText=[Text "Test link"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Link with no title"
        )

        (
        "[Test](www.google.com)(Some Title)",
        [Link {linkText=[Text "Test"];
               linkDest=[Text "www.google.com"];
               linkTitle=Some [Text "Some Title"]}],
        "Link with title"
        )

        (
        "[](www.google.com)",
        [Link {linkText=[];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Link with no text and no title"
        )

        (
        "[my link]()",
        [Link {linkText=[Text "my link"];
               linkDest=[]
               linkTitle=None}],
        "Link with no dest and no title"
        )

        (
        "[this is a [test] link](www.imperial.ac.uk)(imperial website)",
        [Link {linkText=[Text "this is a [test] link"];
               linkDest=[Text "www.imperial.ac.uk"];
               linkTitle=Some [Text "imperial website"]}],
        "Link with inner square brackets"
        )

        (
        "[[Imperial College](https://tested.com)",
        [Text "[" ;Link {linkText =[Text "Imperial College"];
                          linkDest =[Text "https://tested.com"];
                          linkTitle= None}],
        "Link text [] edge case"
        )

        (
        "[Test link](   www.google.com)",
        [Link {linkText=[Text "Test link"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Link with leading whitespace"
        )

        (
        "[Test link](www.google.com   )",
        [Link {linkText=[Text "Test link"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Link with trailing whitespace"
        )

        (
        "[Test link](   www.google.com   )",
        [Link {linkText=[Text "Test link"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Link with leading and trailing whitespace"
        )

    ]

    ("Link Parsing Positive",inlineParser,tests) |||> testOfList

[<Tests>]
let linkTestNegative =
    let tests = [
        (
        "[No destination!]",
        [Text "[No destination!]"],
        "Link missing dest"
        )

        (
        "[Imperial College](one two)",
        [Text "[Imperial College](one two)" ],
        "Link destination with more than 1 literal"
        )

        (
        "[Imperial College]](http://www.yahoo.com)",
        [Text "[Imperial College]](http://www.yahoo.com)"],
        "Link text [] not balanced"
        )
    ]

    ("Link Parsing Negative",inlineParser,tests) |||> testOfList

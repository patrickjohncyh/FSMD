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

//test for empty string         


[<Tests>]
let textTest =
    let tests = [
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
    



//[<Tests>]
//let linkTest =
    //let tests = [
    //    (
    //    "[Test link](www.google.com)",
    //    [Link "Hello"],
    //    "Link with no title"
    //    )

    //    ("Hello World this is some text with numbers 1 2 3 4",
    //     [Text "Hello World this is some text with numbers 1 2 3 4"],
    //     "Sentence")

    //    (
    //    "! @ # $ % ^ & * ( ) _ +",
    //    [Text "! @ # $ % ^ & * ( ) _ +"],
    //    "Symbols"
    //    )

    //    (
    //    "! @ # T]e[[st $ > \\ Rand()m > ~!! * ! @... <x<x a_b_c \"Quoted\" ", 
    //    [Text "! @ # T]e[[st $ > \\ Rand()m > ~!! * ! @... <x<x a_b_c \"Quoted\" "],
    //    "Symbols and Text"
    //    )
    //]

    //("Text Parsing",inlineParser,tests) |||> testOfList

    // Basic Tests

    // Link no title
    // [Test link](www.google.com)
 
    // Link with title
    // [Tes](www.google.com)(Title)

    // Link no text
    // [](www.google.com)

    // Link no explicit dest
    // [Test link)()


    // Link with inner sq brackets
    // [Test[link]](google.com)

    // Harder Tests

    // Link missing dest
    // [Test]

    // Link dest more than 1 literal
    // [Test](one two)


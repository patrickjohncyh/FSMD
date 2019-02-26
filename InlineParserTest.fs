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
        "Link text [[...]"
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

        (
        "[Imperial College London](http://www.yahoo.com) (not a title)",
        [Link {linkText=[Text "Imperial College London"];
                linkDest=[Text "http://www.yahoo.com"];
                linkTitle=None};
         Text " (not a title)"],
        "Link space between dest and title"
        )

        (
        "[Imperial College](http://www.yahoo.com)(not a title",
         [Link {linkText=[Text "Imperial College"];
                 linkDest=[Text "http://www.yahoo.com"];
                 linkTitle=None};
          Text "(not a title"],
         "Link incomplete title"
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

        (
        "[Imperial College] (http://www.yahoo.com)",
        [Text "[Imperial College] (http://www.yahoo.com)"],
        "Link space between [] and ()"
        )

    ]

    ("Link Parsing Negative",inlineParser,tests) |||> testOfList

[<Tests>]
let breakTests =
    let tests = [
        (
         "Testing the soft\nBreak Parsing",
         [Text "Testing the soft";
         Softbreak;
         Text "Break Parsing"],
         "Basic soft break, 0 Whitespace"
        )

        (
         "Testing the soft \nBreak Parsing",
         [Text "Testing the soft";
         Softbreak;
         Text "Break Parsing"],
         "Basic soft break. 1 Whitespace"
        )

        (
         "Testing the hard  \nBreak Parsing",
         [Text "Testing the hard";
         Hardbreak;
         Text "Break Parsing"],
         "Basic hard break"
        )

        (
         "Testing the Hard\\\nBreak Parsing with backslash",
         [Text "Testing the Hard";
         Hardbreak;
         Text "Break Parsing with backslash"],
         "Basic hard break with backslash"
        )

        (
         "Testing the soft \n         Break Parsing with white space on next line",
         [Text "Testing the soft";
         Softbreak;
         Text "Break Parsing with white space on next line"],
         "Soft break with trailing white space"
        )

        (
         "Testing the hard       \n         Break Parsing with extra white space",
         [Text "Testing the hard";
         Hardbreak;
         Text "Break Parsing with extra white space"],
         "Hard break with extra white space"
        )

    ]
    ("Break Parsing",inlineParser,tests) |||> testOfList

[<Tests>]
let codeSpanTests =
    let tests = [
        (
            "`code span contents`",
            [CodeSpan [(Text "code span contents")]],
            "Basic code span"
        )

        (
            "```int main { return 0; }```",
            [CodeSpan [(Text "int main { return 0; }")]],
            "Code span with multiple matching backticks"
        )
        
        (
            "``[link in a code span](test.com)``",
            [CodeSpan [(Text "[link in a code span](test.com)")]],
            "Code span with 'styled' elements"
        )

        (
            "``Some ranodm`text ``",
            [CodeSpan [(Text "Some ranodm`text ")]],
            "Code span with non-matching backtick inside"
        )

        (
            "`Some random``text`",
            [CodeSpan [(Text "Some random``text")]],
            "Code span with non-matching backtick inside"
        )

        (
            "`Some random` `text`",
            [CodeSpan [(Text "Some random")];
            Text(" ");
            CodeSpan [(Text "text")]],
            "Two code spans"
        )
    ]
    ("Code Span Parsing",inlineParser,tests) |||> testOfList


[<Tests>]
let emphasisTests =
    let tests = [
        (
            "*(italics text)*",
            [Emphasis [(Text "italics text")]],
            "Basic emphasis asterisk"
        )

        (
            "_(italics text)_",
            [Emphasis [(Text "italics text")]],
            "Basic emphasis underscore"
        )

        (
            "*(*(italics text)*)*",
            [Emphasis [Emphasis [(Text "italics text")]]],
            "Emphasis nested asterisk"
        )

        (
            "_(_(italics text)_)_",

            [Emphasis [Emphasis [(Text "italics text")]]],
            "Emphasis nested underscore"
        )

        (
            "*(italics text)* and more _(underscore text)_",
            [Emphasis [Text "italics text"];
             Text " and more ";
             Emphasis [Text "underscore text"]],
            "Emphasis multiple mixed"
        )
    ]

    ("Emphasis Parsing",inlineParser,tests) |||> testOfList


[<Tests>]
let strongTests =
    let tests = [
        (
            "**(italics text)**",
            [Strong [(Text "italics text")]],
            "Basic strong asterisk"
        )

        (
            "__(italics text)__",
            [Strong [(Text "italics text")]],
            "Basic strong underscore"
        )

        (
            "**(**(italics text)**)**",
            [Strong [Strong [(Text "italics text")]]],
            "Strong nested asterisk"
        )

        (
            "__(__(italics text)__)__",

            [Strong [Strong [(Text "italics text")]]],
            "Strong nested underscore"
        )

        (
            "**(italics text)** and more __(underscore text)__",
            [Strong [Text "italics text"];
             Text " and more ";
             Strong [Text "underscore text"]],
            "Strong multiple mixed"
        )
    ]

    ("Strong Parsing",inlineParser,tests) |||> testOfList

[<Tests>]
let imageTestPositive =
    let tests = [
        (
        "![Test img](www.google.com/images/test.png)",
        [Image {linkText=[Text "Test img"];
               linkDest=[Text "www.google.com/images/test.png"];
               linkTitle=None}],
        "Image with no title"
        )

        (
        "![Test](www.google.com)(Some Title)",
        [Image {linkText=[Text "Test"];
               linkDest=[Text "www.google.com"];
               linkTitle=Some [Text "Some Title"]}],
        "Image with title"
        )

        (
        "![](www.google.com)",
        [Image {linkText=[];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Image with no text and no title"
        )

        (
        "![my img]()",
        [Image {linkText=[Text "my img"];
               linkDest=[];
               linkTitle=None}],
        "Image with no dest and no title"
        )

        (
        "![this is a [test] img](www.imperial.ac.uk/logo.png)(imperial logo)",
        [Image {linkText=[Text "this is a [test] img"];
               linkDest=[Text "www.imperial.ac.uk/logo.png"];
               linkTitle=Some [Text "imperial logo"]}],
        "Image with inner square brackets"
        )

        (
        "[![Imperial College](https://tested.com/image.png)",
        [Text "[" ;Image {linkText =[Text "Imperial College"];
                          linkDest =[Text "https://tested.com/image.png"];
                          linkTitle= None}],
        "Image text [![...]"
        )

        (
        "![Test image](   www.google.com)",
        [Image {linkText=[Text "Test image"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Image with leading whitespace"
        )

        (
        "![Test img](www.google.com   )",
        [Image {linkText=[Text "Test img"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Image with trailing whitespace"
        )

        (
        "![Test image](   www.google.com   )",
        [Image {linkText=[Text "Test image"];
               linkDest=[Text "www.google.com"];
               linkTitle=None}],
        "Image with leading and trailing whitespace"
        )

        (
        "![Imperial College London](http://www.yahoo.com) (not a title)",
        [Image {linkText=[Text "Imperial College London"];
                linkDest=[Text "http://www.yahoo.com"];
                linkTitle=None};
         Text " (not a title)"],
        "Image space between dest and title"
        )

        (
        "![Imperial College](http://www.yahoo.com)(not a title",
         [Image {linkText=[Text "Imperial College"];
                 linkDest=[Text "http://www.yahoo.com"];
                 linkTitle=None};
          Text "(not a title"],
         "Image incomplete title"
         )

    ]

    ("Image Parsing Positive",inlineParser,tests) |||> testOfList

[<Tests>]
let imageTestNegative =
    let tests = [
        (
        "![No destination!]",
        [Text "![No destination!]"],
        "Image missing dest"
        )

        (
        "![Imperial College](one two)",
        [Text "![Imperial College](one two)" ],
        "Image destination with more than 1 literal"
        )

        (
        "![Imperial College]](http://www.yahoo.com)",
        [Text "![Imperial College]](http://www.yahoo.com)"],
        "Image text [] not balanced"
        )

        (
        "![Imperial College] (http://www.yahoo.com)",
        [Text "![Imperial College] (http://www.yahoo.com)"],
        "Image space between [] and ()"
        )

    ]

    ("Image Parsing Negative",inlineParser,tests) |||> testOfList
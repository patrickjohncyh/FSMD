module InlineParserTest

open System
open Expecto
open FsCheck

open Types
open InlineParserHelpers
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

(*
    Test case format
    ----------------
    ( 
    Input...,
    Expected ...,
    Info ...
    )

*)

[<Tests>]
let tokeniserTest =
    let tests = [
        (
            "[] ()",
            [SBracketO;SBracketC;Whitespace;RBracketO;RBracketC],
            "Square Brackets, Whitespace, Round Brackets"
        )

        (
            "\"!* *()* _()_",
            [QuoteMark;Exclamation;Asterisk;Whitespace
             EmpOpenAst;EmpCloseAst;Whitespace;EmpOpenUnd;EmpCloseUnd],
             "Quotemark, Exclamation, Asteris, Emphasis Delimiters"
        )

        (
            "**()**__()___<>\\",
            [StrongOpenAst;StrongCloseAst;StrongOpenUnd;StrongCloseUnd
             Underscore;LessThan;MoreThan;Backslash],
             "Strong Delimiters, Less Than, More Than ,Backslash"
        )

        (
            "Hello World, Goodbye World.",
            [Literal "Hello";Whitespace;Literal "World";Literal ",";Whitespace;
             Literal "Goodbye";Whitespace;Literal "World";Literal "."],
             "Literal tokenisation"
        )

        (
            "``` ` `` ```` ` `` ````````",
            [BacktickStr 3;Whitespace;BacktickStr 1;Whitespace;BacktickStr 2;Whitespace
             BacktickStr 4;Whitespace;BacktickStr 1;Whitespace;BacktickStr 2;Whitespace
             BacktickStr 8],
             "Backticks of various lengths"
        )

        (
            "`let a = 5`. This is _(F#)_ syntax",
            [BacktickStr 1;Literal "let";Whitespace;Literal "a";Whitespace;Literal "=";Whitespace;
             Literal "5" ;BacktickStr 1;Literal ".";Whitespace; Literal "This";Whitespace;
             Literal "is";Whitespace;EmpOpenUnd;Literal "F";Literal "#";
             EmpCloseUnd;Whitespace;Literal "syntax"],
             "Mixed Tokens"


        )
    ]

    ("Tokeniser",inlineTokeniser,tests) |||> testOfList



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
        

        (
            "Hello World this is some text with numbers 1 2 3 4",
             [Text "Hello World this is some text with numbers 1 2 3 4"],
             "Sentence"
         )

        (
            "! @ # $ % ^ & * ( ) _ +",
            [Text "! @ # $ % ^ & * ( ) _ +"],
            "Symbols"
        )

        (
            "\! \" \# \$ \% \& \' \( \* \+ \, \- \. \/ \: \; \< \= \> \? \@ \[ \\\\ \] \^ \_ \' \{ \| \} \~",
            [Text "! \" # $ % & ' ( * + , - . / : ; < = > ? @ [ \\ ] ^ _ ' { | } ~"],
            "Escaped Punctuation"
        )

        (
            "! @ # T]e[[st $ > \\ Rand()m > ~!! * ! @... <x<x a_b_c \"Quoted\" ", 
            [Text "! @ # T]e[[st $ > \\ Rand()m > ~!! * ! @... <x<x a_b_c \"Quoted\" "],
            "Symbols and Text"
        )
    ]
    ("Text Parsing",inlineParser,tests) |||> testOfList

[<Tests>]
let linkTests =
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
    ("Link Parsing",inlineParser,tests) |||> testOfList

[<Tests>]
let linkRefTest = 
    let tests = [
        (
            "[Some title][Some reference]",
            [LinkRef {linkText = "Some title";
                      linkRef  = Some "Some reference"}],
            "Basic link reference"
        )

        (
            "[Some title][]",
            [LinkRef {linkText = "Some title";
                      linkRef  = None}],
            "Basic link reference, no reference"
        )

        (
            "[Some title]",
            [LinkRef {linkText = "Some title";
                      linkRef  = None}],
            "Shortcut link reference"
        )

        (
            "[Imperial College](one two)",
            [LinkRef {linkText = "Imperial College";
                      linkRef  = None};
             Text "(one two)"],
            "Shortcut when normal link fails"
        )

        (
            "[Imperial College]](http://www.yahoo.com)",
            [LinkRef {linkText = "Imperial College";
                      linkRef  = None};
             Text "](http://www.yahoo.com)"],
            "Link ref when Link text [] not balanced"
        )

        (
            "[Imperial College] (http://www.yahoo.com)",
            [LinkRef {linkText = "Imperial College";
                      linkRef  = None};
             Text " (http://www.yahoo.com)"],
            "LinkRef when Link has space between [] and ()"
        )
    ]
    ("LinkRef Parsing",inlineParser,tests) |||> testOfList

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
let codeSpanTest =
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
let imageTest =
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
    ("Image Parsing",inlineParser,tests) |||> testOfList

[<Tests>]
let imageRefTest =
    let tests = [
        (
            "![Some title][Some reference]",
            [ImageRef {linkText = "Some title";
                       linkRef  = Some "Some reference"}],
            "Basic image reference"
        )

        (
            "![Some title][]",
            [ImageRef {linkText = "Some title";
                       linkRef  = None}],
            "Basic image reference no ref"
        )

        (
            "![Some title][ref]",
            [ImageRef {linkText = "Some title";
                       linkRef  = Some "ref"}],
            "Shortcut image reference"
        )

        (
            "![Imperial College](one two)",
            [ImageRef {linkText = "Imperial College";
                       linkRef  = None};
             Text "(one two)"],
            "Shortcut when normal image fails"
        )

        (
            "![Imperial College]](http://www.yahoo.com)",
            [ImageRef {linkText = "Imperial College";
                       linkRef  = None}
             Text "](http://www.yahoo.com)"],
            "Image ref when Image text [] not balanced"
        )

        (
            "[Imperial College] (http://www.yahoo.com)",
            [LinkRef {linkText = "Imperial College";
                      linkRef  = None}
             Text " (http://www.yahoo.com)"],
            "Image ref when Image has space between [] and ()"
        )
    ]
    ("Image Ref Parsing",inlineParser,tests) |||> testOfList


[<Tests>]
let MixedStylesTest =
    let tests = [
        (
            "Emphasising some *([My link](google.com))*",
            [Text "Emphasising some ";
             Emphasis [Link {linkText = [Text "My link"];
                             linkDest = [Text "google.com"];
                             linkTitle= None} ]],
            "Emphasising a Link"
        )

        (
            "Link with [**(Escaped)** punctuation \! and `Code span inside!!`](google.com)",
            [Text "Link with "
             Link {linkText = [Strong [Text "Escaped"]
                               Text " punctuation ! and "
                               CodeSpan [Text "Code span inside!!"]];
                   linkDest = [Text "google.com"];
                   linkTitle= None}],
            "Link with styled text"
        )

        (
            "`let a = 5`. This is F# syntax  \n    where a is immutable\n   and has type `int`",
             [CodeSpan [Text "let a = 5"]
              Text ". This is F# syntax"
              Hardbreak
              Text "where a is immutable"
              Softbreak
              Text "and has type "
              CodeSpan [Text "int"]],
             "Mixing code span and breaks"
        )

        (
            "Our device SUSH uses the Spectral Sensor ([AS7262](https://ams.com/as7262)) provided in conjunction with a Proximity Sensor (![VCNL4010](https://www.vishay.com/doc?83462))",
            [Text "Our device SUSH uses the Spectral Sensor ("
             Link {linkText = [Text "AS7262"];
                   linkDest = [Text "https://ams.com/as7262"];
                   linkTitle= None};
             Text ") provided in conjunction with a Proximity Sensor ("
             Image {linkText = [Text "VCNL4010"];
                   linkDest = [Text "https://www.vishay.com/doc?83462"];
                   linkTitle= None};
             Text ")"],
            "Real usecase example"
        )
    ]
    ("Mixed Styles Parsing",inlineParser,tests) |||> testOfList
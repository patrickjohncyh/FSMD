//===============================================================================================
//                                       START OF TESTING
//===============================================================================================
module BlockParserTests

open BlockParserTypes
open BlockParser
open FsCheck
open Expecto

[<Tests>]
/// RegexPat should only split if a match is found at the beginning of string
let RegexPatCorrectTest =
    testCase "RegexPat testing correct case" <| fun () ->
    let expected = "Hi ","there"
    let str = "Hi there"
    Expect.equal (match str with
                    | RegexPat "Hi " res -> res ) expected "Function splits fine"
             
[<Tests>]
/// RegexPat should fail if a match is found elsewhere
let RegexPatWrongTest =
    testCase "RegexPat testing wrong case" <| fun () ->
    let expected = "testing","failed"
    let str = "Hi there"
    Expect.equal (match str with
                    | RegexPat "the" res -> res
                    | _                  -> "testing","failed") expected "Function should fail"

[<Tests>]
let LRefTextIdTest1 =
    testCase "LRefTextId testing correct case" <| fun () ->
    let expected = "linkEx","www.gmail.com 'title'"
    let str      = "[linkEx]:www.gmail.com 'title'"
    Expect.equal(match str with
                 | LRefTextId tst -> tst) expected "function should succeed"

[<Tests>]
let LRefTextIdTest2 =
    testCase "LRefTextId testing with nested ]" <| fun () ->
    let expected = "[linkEx]","www.gmail.com 'title'"
    let str      = "[[linkEx]]:www.gmail.com 'title'"
    Expect.equal(match str with
                 | LRefTextId tst -> tst) expected "function should succeed"

[<Tests>]
let LRefTextIdTest3 =
    testCase "LRefTextId testing that fails" <| fun () ->
    let expected = "should","fail"
    let str      = "asdf[[linkEx]]:www.gmail.com 'title'"
    Expect.equal(match str with
                 | LRefTextId tst -> tst
                 | _              -> "should","fail") expected "function should fail"

[<Tests>]
let LRefURLTest1 =
    testCase "LRefURL extracts URL successfully" <| fun () ->
    let expected = "www.gmail.com"," 'title'"
    let str = "www.gmail.com 'title'"
    Expect.equal(match str with
                 | LRefURLId  tst -> tst
                 | _              -> "should","fail") expected "function should succeed"
 
[<Tests>]
let LRefURLTest2 =
    testCase "LRefURL doesn't extract URL successfully" <| fun () ->
    let expected = "should","fail"
    let str = "'title' alakazam"
    Expect.equal(match str with
                 | LRefURLId  tst -> tst
                 | _              -> "should","fail") expected "function should fail"

[<Tests>]
let LRefTitleTest1 =
    testCase "A valid title exist" <| fun () ->
    let expected = "'this is a title'",""
    let str = "'this is a title'"
    Expect.equal(match str with
                 | LRefTitleId  tst -> tst
                 | LRefNoTitle      -> "no","title"
                 | LRefInvalidT     -> "should","fail") expected "function should fail"
[<Tests>]
let LRefTitleTest2 =
    testCase "Invalid title as quotes do not match" <| fun () ->
    let expected = "should","fail"
    let str = "'this is a title"
    Expect.equal(match str with
                 | LRefTitleId  tst -> tst
                 | LRefNoTitle      -> "no","title"
                 | LRefInvalidT     -> "should","fail") expected "function should fail"
[<Tests>]
let LRefTitleTest3 =
    testCase "Invalid title as there are more characters after the title" <| fun () ->
    let expected = "should","fail"
    let str = "'this is a title' not supposed to be here"
    Expect.equal(match str with
                 | LRefTitleId  tst -> tst
                 | LRefNoTitle      -> "no","title"
                 | LRefInvalidT     -> "should","fail") expected "function should fail"
[<Tests>]
let LRefTitleTest4 =
    testCase "No title as it's only whitespace" <| fun () ->
    let expected = "no","title"
    let str = "       "
    Expect.equal(match str with
                 | LRefTitleId  tst -> tst
                 | LRefNoTitle      -> "no","title"
                 | LRefInvalidT     -> "should","fail") expected "function should succeed"
[<Tests>]
let LRefHandlerTest1 =
    testCase "A valid lRefDec rawBlock" <| fun () ->
    let expected = Ok (LRefD {lText="fooo" ;lURL="/url" ;lTitle= Some "title1"})
    let blockTest = {blocktype=LRefDec; mData="[fooo]: /url \"title1\""}
    Expect.equal(lRefHandler blockTest) expected "function should succeed"
[<Tests>]
let LRefHandlerTest2 =
    testCase "A valid lRefDec rawBlock with no title" <| fun () ->
    let expected = Ok (LRefD {lText="fooo" ;lURL="/url" ;lTitle= None})
    let blockTest = {blocktype=LRefDec; mData="[fooo]: /url"}
    Expect.equal(lRefHandler blockTest) expected "function should succeed"
[<Tests>]
let LRefHandlerTest3 =
    testCase "An invalid title lRefDec rawBlock as there are non-whitespace characters after title" <| fun () ->
    let expected = Error <|sprintf "Invalid title"
    let blockTest = {blocktype=LRefDec; mData="[fooo]: /url \"title1\"fsdfs"}
    Expect.equal(lRefHandler blockTest) expected "function should return error message"
[<Tests>]
let lRefHandlerTest4 =
    testCase "An invalid title lRefDec due to unmatched quotation marks" <| fun () ->
    let expected = Error <|sprintf "Invalid title"
    let blockTest = {blocktype=LRefDec; mData="[fooo]: /url \"title1'"}
    Expect.equal(lRefHandler blockTest) expected "function should return error message"
[<Tests>]
let lRefHandlerTest5=
    testCase "A valid title over multiple lines" <| fun() ->
    let expected = Ok (LRefD {lText = "fooo";
                       lURL = "/url";
                       lTitle = Some "title1";})
    let blockTest = {blocktype=LRefDec;
   mData=
"[fooo]: 
/url
\"title1\""}
    Expect.equal(lRefHandler blockTest) expected "function should succeed"

let testListWithExpecto =
    testList "A test group" [
    RegexPatCorrectTest
    RegexPatWrongTest
    ]
let testsWithExpecto() =
        runTests defaultConfig testListWithExpecto |> ignore
let allTestsWithExpecto() =
        runTestsInAssembly defaultConfig [||]
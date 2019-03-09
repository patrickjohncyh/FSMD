//===============================================================================================
//                                       START OF TESTING
//===============================================================================================
module BlockParserTests

open Types
open BlockParser
open FsCheck
open Expecto

// to use this in the future, more generalised unit testing
// code from Patrick
let makeTestCase testFn testName (testInput,expOutput,msg) = 
    testCase testName <| fun () ->
        Expect.equal (testFn testInput)  expOutput msg

let testOfList groupName testFn testTripList = 
    testTripList 
    |> List.indexed
    |> List.map (fun (idx,triple) -> makeTestCase testFn (groupName+"_"+string(idx)) triple)
    |> testList groupName
[<Tests>]
/// RegexPat should only split if a match is found at the beginning of string
let RegexPatCorrectTest =
    testCase "RegexPat testing correct case" <| fun () ->
    let expected = "Hi ","there"
    let str = "Hi there"
    Expect.equal (match str with
                    | RegexPat "Hi " res -> res
                    | _                  -> "testing","failed") expected "Function splits fine"
             
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
let blockIdentifierTest1 =
    testCase "A Heading 1 is observed, blockIdentifier returns appropriately" <| fun() ->
    let expected = (Heading1,"This is the content")
    let input = "# This is the content"
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest2 =
    testCase "A Heading 2 is observed, blockIdentifier returns appropriately" <| fun() ->
    let expected = (Heading2,"This is the content")
    let input = "## This is the content"
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest3 =
    testCase "A Heading 3 is observed, blockIdentifier returns appropriately" <| fun() ->
    let expected = (Heading3,"This is the content")
    let input = "### This is the content"
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest4 =
    testCase "A Heading 4 is observed, blockIdentifier returns appropriately" <| fun() ->
    let expected = (Heading4,"This is the content")
    let input = "#### This is the content"
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed" 
[<Tests>]
let blockIdentifierTest5 =
    testCase "A Heading 5 is observed, blockIdentifier returns appropriately" <| fun() ->
    let expected = (Heading5,"This is the content")
    let input = "##### This is the content"
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest6 =
    testCase "A Heading 6 is observed, blockIdentifier returns appropriately" <| fun() ->
    let expected = (Heading6,"This is the content")
    let input = "###### This is the content"
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest7 =
    testCase "More than 6# is observed, get parsed as a paragraph" <| fun() ->
    let input = "####### This is the content"
    let expected = (Para,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"  
[<Tests>]
let blockIdentifierTest8 =
    testCase "A line of blockquote is observed" <| fun() ->
    let input = ">   This is parsed as blockquote"
    let expected = (BlockQuote,"This is parsed as blockquote")
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest9 =
    testCase "A line of list item is observed ." <| fun() ->
    let input = "1. This is parsed as list"
    let expected = (List,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest10 =
    testCase "A line of list item is observed )" <| fun() ->
    let input = "1) This is parsed as list "
    let expected = (List,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest11 =
    testCase "A line of list item is observed *" <| fun() ->
    let input = "* This is parsed as list"
    let expected = (List,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest12 =
    testCase "A line of list item is observed -" <| fun() ->
    let input = "- This is parsed as list"
    let expected = (List,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest13 =
    testCase "A line of list item is observed +" <| fun() ->
    let input = "+ This is parsed as list"
    let expected = (List,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest14 =
    testCase "A line of codeblock is observed" <| fun() ->
    let input = "    This is parsed as codeblock"
    let expected = (CBlock,"This is parsed as codeblock")
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"   
[<Tests>]
let blockIdentifierTest15 =
    testCase "A line of thematic break is observed" <| fun() ->
    let input = "* * *"
    let expected = (ThematicBreak,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest16 =
    testCase "A line of blankline" <| fun() ->
    let input = ""
    let expected = (BlankLine,"")
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest17 =
    testCase "A line of Link Reference Declaration" <| fun() ->
    let input = "[linktext]: /url \"gagagag\""
    let expected = (LRefDecB,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest18 =
    testCase "A line of Table is observed" <| fun() ->
    let input = "| Name | Age | Sex |"
    let expected = (TableBlock,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"
[<Tests>]
let blockIdentifierTest19 =
    testCase "A line of Table is observed, first column at least 3 \"-\"" <| fun() ->
    let input = "| --- | ---- | ----- |"
    let expected = (TableBlock,input)
    Expect.equal(match input with
                 |BlockIdentifier result -> result) expected "function should succeed"

[<Tests>]
//REVISIT:In F# interactive this succeed although not in Expecto
//For some reason Expecto.equal does not remove the newline character
let lRefHandlerTest5=
    testCase "A valid title over multiple lines" <| fun() ->
    let expected = Ok (LRefD {lText="fooo" ;lURL="/url" ;lTitle= Some "title1"})
    let blockTest = {blocktype=LRefDec;
   mData=
"[fooo]:
/url\"title1\""}
    Expect.equal(lRefHandler blockTest) expected "function should succeed"

[<Tests>]
//REVISIT: Facing the same issue as previous test
let blockParserTest=
    testCase "checking blockParser" <| fun () ->
    let input =   Some (["[fooo]: "; "ulalalalal"; "'title of the link'"; "[f2]:";
     "heyeyeyey \"another link\""; ""; "[f4]: yoyoy"; "[f3]:"; "heheheh '"; "";
     "invalied url'"; "[fooo] [f2]"; ""; "> This is a blockquote";
     "continuation it should be"; ""; "# This is heading 1"; "";
     "    This is a codeblock"; "    as it is indented 4 space"; ""; "*** ***";
     "I just put a thematic break up there"; ""; "1. List item one";
     "2. List item 2"; "   * Sublist item 1"; "3. List item 3"; "";
     "This text right here will"; "be a setex heading as I put = below"; "===";
     ""; "This will be a table"; "| Name | Age  |"; "| ---  | ---  |";
     "| And  | 30   |"; "";
     "Blanklines are going to be removed by this block parser"; "";
     "\# the heading will be esaped as I'm using the escape thing"])
    let expected =  Ok([{blocktype = Para;
       mData = "[f3]:
heheheh '";}; {blocktype = Para;
                                     mData = "invalied url'
[fooo] [f2]";};
      {blocktype = BlockQuote;
       mData = "This is a blockquote
continuation it should be";};
      {blocktype = Heading1;
       mData = "This is heading 1";};
      {blocktype = CBlock;
       mData = "This is a codeblock
as it is indented 4 space";};
      {blocktype = ThematicBreak;
       mData = "*** ***";}; {blocktype = Para;
                             mData = "I just put a thematic break up there";};
      {blocktype = List;
       mData =
        "1. List item one
2. List item 2
   * Sublist item 1
3. List item 3";};
      {blocktype = Heading1;
       mData =
        "This text right here will
be a setex heading as I put = below";};
      {blocktype = Para;
       mData = "This will be a table";};
      {blocktype = TableBlock;
       mData = "| Name | Age  |
| ---  | ---  |
| And  | 30   |";};
      {blocktype = Para;
       mData = "Blanklines are going to be removed by this block parser";};
      {blocktype = Para;
       mData = "# the heading will be esaped as I'm using the escape thing";}],
     [{lText = "fooo";
       lURL = "ulalalalal";
       lTitle = Some "title of the link";}; {lText = "f2";
                                             lURL = "heyeyeyey";
                                             lTitle = Some "another link";};
      {lText = "f4";
       lURL = "yoyoy";
       lTitle = None;}])
    Expect.equal(blockParser input) expected "function should succeed"



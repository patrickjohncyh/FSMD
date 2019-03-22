module TableHandlerTest

open Types
open TableHandler

open Expecto
open FsCheck

let createTest func testName ( input,expected,info) = 
    testCase testName <| fun () ->
        Expect.equal (func input) expected info

let listTest groupName func inputTriple = 
    inputTriple 
    |> List.indexed
    |> List.map (fun (idx,triple) -> createTest func (groupName+"_"+string(idx)) triple)
    |> testList groupName


[<Tests>]
let testTableTokeniser =
    let tests = [
        (
            "| aB1 | cD2 | eF3 |",                              //input
            [Cell "| aB1 ";Cell "| cD2 ";Cell "| eF3 "],        //expected
            "Tokenising alphanumeric cells "                    //test information
        )

        (
            "| --- | ----- | ----------- |",
            [DelimCell;DelimCell;DelimCell],
            "Tokenising Delimiter Cells "
        )

        (
            "| !@#$%^& | \| | *()_+ |",
            [Cell "| !@#$%^& ";Cell "| \| ";Cell "| *()_+ "],
            "Tokenising escaped Characters - Middle Row "
        )   

        (
            "| Name | Age | ----------- | \n | A | 22 |\n| B | 33 |",
            [Cell "| Name ";Cell "| Age ";DelimCell;NewRow;Cell "| A ";Cell "| 22 ";NewRow;Cell "| B ";Cell "| 33 "],
            "Tokenising NewRow - testing multi row detection "
        )


    ]

    ("Tokeniser",tableTokeniser,tests) |||> listTest



[<Tests>]
let testTableParser =
    let tests = [
        (
            [Cell "| Name ";Cell "| Age ";DelimCell;NewRow;Cell "| A ";Cell "| 22 "],
            ([[Text "Name"]; [Text "Age"]],[[[Text "A"]; [Text "22"]]]) ,
            "Parsing alphanumeric table with 1 body row "
        )

        (
            [Cell "| Name ";Cell "| Age ";DelimCell;NewRow;Cell "| A ";Cell "| 22 ";NewRow;Cell "| B ";Cell "| 33 "],
            ([[Text "Name"]; [Text "Age"]],[[[Text "A"]; [Text "22"]];[[Text "B"]; [Text "33"]]]) ,
            "Parsing alphanumeric table with multi body row"
        )

        (
            [Cell "| \|\| ";Cell "| @#$% ";DelimCell;NewRow;Cell "| -=_+ ";Cell "| ^&*() "],
            ([[Text "\|\|"]; [Text "@#$%"]],[[[Text "-=_+"]; [Text "^&*()"]]])  ,
            "Parsing table with escaped characters"
        )
 

    ]

    ("Parser",tableParser,tests) |||> listTest
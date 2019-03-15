module BlockHandlerTest

//open System
//open Expecto
//open BlockHandler

////testCases for listBlockHandler (Parser side)
//let testCase1 = "1. one\n2. two\n3. three"                                                                              // base case
//let expected1 = [[(0, ("1.", "one")); (0, ("2.", "two")); (0, ("3.", "three"))]]

//let testCase2 = "1. one\n   2. two\n   3. three\n  4. four    \n     5. five"                                           //improper indentation
//let expected2 = [[(0, ("1.", "one")); (1, ("   2.", "two")); (1, ("   3.", "three"));(0, ("4.", "four    ")); (1, ("   5.", "five"))]]

//let testCase3 = "   1. one\n      2. two\n   3. three\n      4. four    \n5. five"                                      //first line indent
//let expected3 = [[(0, ("1.", "one")); (1, ("   2.", "two")); (0, ("3.", "three"));(1, ("   4.", "four    ")); (0, ("5.", "five"))]]

//let testCase4 = "   1. one\nappendtolast\n2. two\n   3. three\n      4. four    \n5. five"                              //append to last
//let expected4 = [[(0, ("1.", "one appendtolast")); (0, ("2.", "two")); (0, ("3.", "three"));(1, ("   4.", "four    ")); (0, ("5.", "five"))]]

//let testCase5 = "1. one\nappendtolast\n   2. two\nappendtolast\n   3. three\n      4. four    \n5. five"                //append to last within sublist
//let expected5 = [[(0, ("1.", "one appendtolast")); (1, ("   2.", "two appendtolast"));(1, ("   3.", "three")); (2, ("      4.", "four    "));(0, ("5.", "five"))]]

//let testCase6 = "* one\nappendtolast\n   * two\nappendtolast\n   * three\n      * four    \n* five"                     //other listTyples
//let expected6 = [[(0, ("*", "one appendtolast")); (1, ("   *", "two appendtolast"));(1, ("   *", "three")); (2, ("      *", "four    ")); (0, ("*", "five"))]]

//let testCase7 = "* one\nappendtolast\n   *two\nappendtolast\n   * three\n      4. four    \n5. five"                    //missing space with multiple listTypes
//let expected7 = [[(0, ("*", "one appendtolast    *two appendtolast"));(1, ("   *", "three"))];[(2, ("      4.", "four    ")); (0, ("5.", "five"))]]

//let testCase8 = "*\n* two\nappendtolast\n   * three\n      4. four    \n5. five"                                        //missing content
//let expected8 = [[(0, ("*", "")); (0, ("*", "two appendtolast")); (1, ("   *", "three"))];[(2, ("      4.", "four    ")); (0, ("5.", "five"))]]

//[<Tests>]
//let listTest (inputString: string) (expectedOutput: (int*(string*string))list list)  =
//    testCase "listBlockHandler test" <| fun () ->
//        Expect.equal (listBlockHandlerTest inputString) (expectedOutput) ""

//let testsWithExpecto (inputString: string) (expectedOutput: (int*(string*string))list list) =
//    runTests defaultConfig (listTest inputString expectedOutput)|> ignore

//[<EntryPoint>]
//let main argv =
    //printfn "Testing with FSCheck and Expecto!"
    //List.map2 testsWithExpecto [testCase1;testCase2;testCase3;testCase4;testCase5;testCase6;testCase7;testCase8] [expected1;expected2;expected3;expected4;expected5;expected6;expected7;expected8] |> ignore
    //Console.ReadKey() |> ignore
    //0
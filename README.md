# Table Handler

```tableHandler : string -> Block``` 

`tableHandler` takes a `string` as input from `blockDispatcher`, and outputs a `Block` type which contains necessary information on the content of each specific cell. Each cell is of type `InlineElement List` that is obtained by calling ``inlineParser``
```
type Block =
    | Table of TableCells                

and TableCells = {
    headerRow : InlineElement List List;
    bodyRows : InlineElement List List List
}
```

## Module Ordering
The module ordering and brief details is as follows: 
- TableTypes.fs
	- Declaration of D.U's and Record Types
- TableHandler.fs
	- Main implementation with 2 major functions: tableTokeniser and tableParser
- TableHandlerTest.fs
    - Testbench for TableHandler


## Program Flow

Two main stages:
1. Tokenisation
2. Parsing

#### 1. tableTokeniser 
`tableTokeniser: string -> TableTokens List`

Example of an input table string:
```
| Name          | Age           |
| ------------- | ------------- | 
| Adam          | 10            | 
```
Tokenising is important as it makes parsing much easier by identifying delimiters, the type of cell, and the start and end of a cell or row. `tableTokeniser` is a recursive function that uses Regex and Partial Active Patterns to convert the entire `string` input into a `List` of the following `TableToken` types: 
1. `NewRow` -> separates rows by identifying newline operator.
2. `DelimCell` -> separates body and header rows. Minimum of 3 "-".   E.g. | - - - - |
3. `Cell of string` -> stores the content of non-newline, non-delimiter cells.  E.g. | Name |

Example Output of tableTokeniser:
```
[Cell "| Name          " ; Cell "| Age           " ; DelimCell; DelimCell; Cell....]
```
#### 2. tableParser
`tableParser : TableToken list -> (InlineElement List List * InlineElement List List List)`



##### (tableToken List) as input to tableParser will then be split into (headerList * bodyList) using the following subfunctions of tableParser:

`(|FirstCellIndexOf|_|): active recognizer` 
= uses partial active pattern to determine the first index of a requested `TableToken`, in a given `TableToken List`. `List.tryFindIndex` is a way better alternative to `List.findIndex` to return `int` index values, in case token is not found in the list which ends the program.
___
`splitIntoRows: TableToken List -> TableToken List List` 
= uses `FirstCellIndexOf` to split input `tableToken list` into header list and body list. Also reused to split body list into lists of body rows. 
___
`extractCellContent: TableToken -> string` 
= Once the splitting of rows are done, individual cells of every row are now extracted for their content. Throwing away unnecessary spacing inside cells and returning `string` content. 
___
`parseCellContent: string -> InlineElement List`
= `string` content of each cell is passed to `inlineParser` for processing. Each cell is returned an `InlineElement List`
___
**Note:** Delimiter Cells | - - -| are key to identifying tables in our version of Markdown. A table **must** have both header and body rows. Minor changes to code will allow tables to be either all header or all body rows but we do not see the benefits of doing that

### Group Stage Additional features
Due to a lack of time, I still intend to add CSS support in Markdown for tables to allow convenient configuration of table size, color, font, etc. Plans of introducing List Blocks inside table cells are still in discussion with team.




## Testing

Testing was performed using Expecto framework to ensure that the tokeniser and parser (tested separately) are capable of handling corner cases such as unique whitespacing, special characters, escaped delimiters and multi row support. This was done using small handwritten unit test cases. 

The following table shows the features tested.

|  Feature Tested | Result |
|:---------------:|:------:|
|     Multi Row Support   | Passed |
|     Unique Whitespacing  | Passed |
|       Special Characters      | Passed |
|      escaped delimiters      | Passed |

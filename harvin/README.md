# Block Parser
``blockParser: string list option -> Result <(RawBlock list*LinkRefD list),string>``
***
``blockParser`` accepts an option type of ``Some string list`` when a markdown text file is successfully converted into ``string list``
``Result<'T,'TError>`` is used to distinguish different kinds of error, as it isn't simply ``Some`` or ``None`` in
several cases.

`blockParser` outputs a tuple of RawBlock list * LinkRefD list on success, as a map of link reference is needed by `inlineparser`
to determine whether a linkText is valid or not.
`RawBlock list` is then given to `BlockDispacther` that will call `BlockHandlers` that are written by other
group members
Below is a flow of execution of the program, on ``Ok Result``:
>
    fPath:string                   string []               string list             RawBlock list*LinkRefD list
    --------------> |fileStream| ------------> |linesList| ------------> |BlockParser| ------------>|BlockDispatcher|
>

The detailed execution flow of ``BlockParser`` is as followed, also on ``Ok Result``:
>

    string list     (BlockId*string)list     RawBlock list               RawBlock list*LinkRefD list
    ---------> |parseLine|---------> |groupBlocks|----------->|LRefCheck|--------->
   
>
### fileStream
The function ``fileStream`` takes in a ``.txt`` file that is plain text written to be parsed as markdown.
Using the library function ``File.ReadAllLine``, each line of the file is read and put into a ``string array``,
which is then converted to ``string list``.
Several exception handlers are written to catch errors, such as file not found, a path is not given, etc

### parseLine
The function parseLine uses several active patterns and partial active patterns to determine
under what block identity each line fall under.

* RegexPat        -> Splits a line of string into 2 first match and rest of string,
will only match if pattern is seen at the beginning
* BlockIdentifier -> Using `RegexPat`, outputs a tuple of `(BlockId,string)` 

Currently supported Blocks are :
1. Heading
9. Paragraph
10. BlockQuote 
11. List
12. CodeBlock
13. ThematicBreak
14. BlankLine
15. LRefDec
16. Table

The `HTML` block parser will be an extension for the group stage.
Each line is then put into a list of `(BlockId,string)` using `List.fold`

### groupBlocks
`groupBlocks` is called recursively, it transforms a list of `(BlockId,string)` into `RawBlock list`
>
    1st iteration:
         NEXT ITEM
    | (BlockId,string) 1 |  (BlockId,string) 2 | (BlockId,string) 3 | (BlockId,string) 4 | .........
            ^
            |
    current accummulated list:
    
    | RawBlock 1|
            ^
            |
    2nd iteration:
          NEXT ITEM
    |  (BlockId,string) 2 | (BlockId,string) 3 | (BlockId,string) 4 |
            ^
            |
    current accummulated list:
    
    | RawBlock 1| RawBlock2 | 
                     ^
                     |  
>
The grouping of blocks follow some rules :
* If next item = current, the content of NEXT ITEM is appended to the current block being observed
* If current item is a container block and NEXT ITEM is a paragraph,
the string content of the paragraph is appended to the current block
* and more, can be seen in the comments of `BlockParser`

I use a lot of pattern match, maybe in the future I will explore some ways to make the grouping of blocks more general

### LRefCheck
When a block is identified as a `LRefDec`, it goes into the function `lRefHandler` which determine whether 
the `LRefDec` block is valid or not, else it needs to be changed to a `Paragraph`.  
A link definition in markdown is  
[linktext]: /url \"title\"   
**OR**  
[linktext]: /url 'title'  
However, a Link Reference Declaration doesn't need to have a title. 
The way I do this is to check one by one, from `LinkText` to `LinkURL` to `LinkTitle`, and whenever
an error is catched that indicates that it is an invalid Link Reference Declaration, the entire
string content is categorised into a `Paragraph` block
This is done by REGEX, active patterns, and partial active patterns.

## Testing
For now, testing is done by simple unit tests from Expecto. In the future, I will add FsCheck to
do randomised testing, which is good in catching corner cases in parsing.

The functions that have been tested are:
* `BlockIdentifier`
* `LRefHandler`
* `BlockParser`

Some problems are faced with Expecto when I try to do unit testing on own record types, for some reason
it works on F# Interactive but doesn't pass in Expecto. This is encountered in BlockParser.
Some problem with whitespace it seems.

Nevertheless, markdown is a language that has many ambiguity in it, which is a price for being a human-readable
HTML generator.

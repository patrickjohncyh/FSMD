module Types

/// BlockParser Types
type BlockId =
    | Heading6  | Heading5 | Heading4
    | Heading3  | Heading2 | Heading1
    | SetexHeading1
    | SetexHeading2
    | ThematicBreak
    | Para
    | BlockQuote
    | CBlock
    | CBlockT
    | BlankLine
    | LRefDec
    | LRefDecB   // Beginning of link reference
    | LRefD of LinkRefD
    | List
    | TableBlock

/// Record type to store link reference declarations
and LinkRefD = {lText: string ; lURL: string ; lTitle : string option}

/// Adding more languages for future works
and Lang     = | Js | Python | Ruby | NoLang 

/// Block record type
type RawBlock = {blocktype: BlockId ; mData: string}




/// InlineParser Types
type InlineElement = 
    | Link      of LinkInfo
    | Image     of LinkInfo
    | CodeSpan  of InlineElement list
    | Strong    of InlineElement list
    | Emphasis  of InlineElement list
    | Text      of string
    | Hardbreak 
    | Softbreak
    | LinkRef   of LinkRefInfo
    | ImageRef  of LinkRefInfo
    | Katex     of string

and LinkInfo = 
    {linkText  : InlineElement list;
     linkDest  : InlineElement list; 
     linkTitle : InlineElement list option} 

and LinkRefInfo =
    {linkText : string
     linkRef  : string option}

type Token = 
    | SBracketO     | SBracketC      | RBracketO     | RBracketC       | QuoteMark  | Whitespace    
    | Newline       | Exclamation    | Asterisk      | EmpOpenAst      | EmpCloseAst| EmpOpenUnd | EmpCloseUnd  
    | StrongOpenAst | StrongCloseAst | StrongOpenUnd | StrongCloseUnd  | Underscore | LessThan   | MoreThan   
    | Backslash     | KatexO         | KatexC
    | Escaped     of string
    | Literal     of string
    | BacktickStr of int
    | Styled      of InlineElement





/// Block Handler Types              
type Block =
    | Paragraph of InlineElement list
    | H1 of InlineElement list
    | H2 of InlineElement list
    | H3 of InlineElement list
    | H4 of InlineElement list
    | H5 of InlineElement list
    | H6 of InlineElement list
    | ListBlock of ListStructure list * string
    | CodeBlock of InlineElement list
    | QuoteBlock of Block list
    | Table of TableCells                   

and ListStructure =
    | InnerList of Block list   // need header
    | ListLines of Block list

and TableCells = {
    headerRow : InlineElement List List;
    bodyRows : InlineElement List List List
}

type ListType =
    | StarList
    | DotList
    | BracketList
    | MinusList
    | PlusList

type TableTokens = 
    | NewRow   
    | DelimCell                   
    | Cell of string
    | CssStyles                 // Not yet implemented   
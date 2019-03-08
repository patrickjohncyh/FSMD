module TableTypes

type Block =
    | Table of TableCells                   //put into generic block handler          

and TableCells = {
    headerRow : InlineElement List List;
    bodyRows : InlineElement List List List
}

and InlineElement =                         //types that are used by InLine Parser
    | Text      of string

type TableTokens = 
    | NewRow   |DelimCell                   // | CssStyles yet to be implemented   
    | Cell of string
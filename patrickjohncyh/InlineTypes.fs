module InlineTypes


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
    | Backslash
    | Escaped     of string
    | Literal     of string
    | BacktickStr of int
    | Styled      of InlineElement
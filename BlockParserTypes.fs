module BlockParserTypes
/// DUs of block identifiers/block tokens
type BlockId =
    | Heading6 
    | Heading5
    | Heading4
    | Heading3
    | Heading2
    | Heading1
    | SetexHeading1
    | SetexHeading2
    | Paragraph
    | BlockQuote 
    | List
    | ListItem
    | CodeBlock
    | ThematicBreak
    | BlankLine
    | LRefDec
    | LRefDecB   // Beginning of link reference
    | LRefD of LinkRefD
    | Table
/// Record type to store link reference declarations
and LinkRefD = {lText: string ; lURL: string ; lTitle : string option}
/// Block record type
type RawBlock = {blocktype: BlockId ; mData: string}
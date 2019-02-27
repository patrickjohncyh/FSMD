# Inline Parser

```inlineParser : string -> InlineElement list``` 

`inlineParser` parses an input string into an `InlineElement list`. Each `InlineElement` represents a different type of inline styling. Each `InlineElement` contains the necceasry information required for conversion into a `DOM Element node` at the final stage of the Markdown Parser.

`inlineParser` will be called by the Block Handlers whenever they require any inline parsing of strings.

## Module Ordering
The module ordering and brief details is as follows: 
- InlineTypes.fs
	- Declaration of D.U's and Record Types
- InlineParserHelpers.fs
	- Helper functions to aid in parsing and tokenising
- InlineParser.fs
	- Main parser implementation
- InlineParserTest.fs
    - Testbench for InlineParser


## Program Flow

Two main steps are performed.
1. Tokenisation 
2. Parsing

```
		   		 string     ---------   Tokens	  --------    InlineElements
			input   ---------> |Tokeniser|  -------> | Parser |  ---------------- > output 
				            ---------	          --------
```

#### Tokenisation

`inlineTokeniser : string -> Token list`

The tokenisation step converts the input string into tokens. This allows for a better representation of characters which may have special significance such as delimiters used for formatting. 

This is achieved with the use of Regex and having the tokeniser recursively call itself on remaining part of the string that is not matched.

Some examples of characters/strings and their corresponding tokens are :

- `* ---> Asterisk`
- `[ ---> SBracketO`
- `` ``` ---> Backtick 3``
- `Hello  ---> Literal "Hello"`






#### Parsing
`parse : Token list -> Token list`

`StyledToInlineElement : Token list -> InlineElement list `

```
                ----         ----    		      ---------------------
Token list --> |S1 P| -..-> |S4 P| --> Token list -->|StyledToInlineElement|--> InlineElement list
                ----	     ----     (Styled only)   ---------------------
                 \_____________/
                      parse
                      
'S1 P' and 'S4 P' are parsers for Style 1 and Style 2 respectively
```


The parsing step performs the detection and conversion of tokens into inline elements. 

In order to enforce a heirarchy of binding of different styles, the parser will first try to parse the entire token list for one style before attempting to parse for another style.

This is achieved by passing the `Token list` specific style parsing functions such as `parseCodeSpans` and `parseLinksOrImg`. Detected styles will be converted into a `Styled` token which is a wrapper for an `InlineElement` and will replace the original tokens which it represents. Wrapping with `Styled` token allows the `Token list` to be passed from one style parser.

The output of `parse` will be a `Token list` of **only** `Styled` tokens. This is then passed into a converter to extract the `InlineElement` from the `Styled` tokens and finally output an `InlineElement list`.




The following inline styles are currently supported : 
* `CodeSpan`
* `Links, Images + References`
* `Emphasis, Strong`
* `Breaks`
* `Text`

Planned subsequent support in group phase:
* `Katex`
* `HTML`

## Testing

Testing was performed using Expecto framework to verify that each supported style is detected and represented correctly. This was done using small handwritten test cases which aim to test both the basic functionality of each style, their pontential variations and corner cases.

The following table shows the features tested.

|  Feature Tested | Result |
|:---------------:|:------:|
|     Codespan    | Passed |
|       Link      | Passed |
|      Image      | Passed |
|  Link Reference | Passed |
| Image Reference | Passed |
|     Emphasis    | Passed |
|      Strong     | Passed |
| Breaks          | Passed |
| Text            | Passed |


module = genericBlockHandler
- All the genericBlockHandlers take in strings from blockDispatcher and returns a “meta DOM” to the blockDispatcher
- The meta DOM structure is block specific and is defined as Blocks which retains all the information needed to reproduce the data as html and has a tree like structure.

listBlockHandler procedure:
1. Breaks strings into list with every newline
2. Check for non label lines to append to previous line
3. Check indent amount for each line
4. Remove indent
5. Append proper indent
6. Check for special case: 1st line indent
7. Separate chunk into separate list (parsing ends here)
8. Check each line 	-> if a list element -> pass content to blockParser
				-> if sublist -> pass to blockParser as a top level list


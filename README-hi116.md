### Harvin Iriawan Individual Statement
Individual Contibutions :
1. Making the `blockParser`, that takes in all lines of strings and convert them into possible blocks to be sent to the `blockDispatcher`,  
also the skeleton for `blockDispatcher`.
2. Stripping down of Visual2 Renderer Code, understanding how the different components work, and only keep what is essential.
3. Configuring monaco editor to do Markdown basic syntax colorisation. (from Monarch).
4. Creating a group README-ALL.md file for documentation purposes.
5. Fixed some internal bugs that was found during group stage  
for example, in codeblock and the fact that \\n and \\r characters must actually be removed.

Challenges :
1. Understanding the interconnectivity between `Renderer` and `Emulator`, as some of the mutable states in the renderer are because  
of mutable values from the emulator. It makes it challanging to strip the functionalities of the more low level `Renderer` code, such as 
`Refs.fs` calling several functions from `Emulator.fs` that aren't easily strips. Just need more time.
2. 
3. Juggling between the other courseworks I suppose.

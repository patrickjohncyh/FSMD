### How the code will be used in team work
The 4 modules that I've written ``BlockParserTypes.fs``, ``BlockParser.fs``, ``BlockParserTests.fs``, ``BlockDispatcher.fs``
are used in the first stage of the team work, where a stream of string is read and 
categorised into different types, line by line.
``BlockDispatcher`` is mostly empty as it essentially calls functions from other group members.
***
These lines are further grouped according to markdown rules into ``RawBlock list`` which will then be taken by ``BlockDispatcher.fs``.
The functions in ``BlockDispatcher.fs`` will call ``BlockHandlers`` and ``InlineHandlers`` in which ``BlockHandlers``
can call ``BlockParser`` again (**mutual recursion**) in the case of container blocks (blocks that can contain another blocks.
***
### How the code interfaces with other team members individual works
Functions that I've written are outputting types that we as a group agreed upon.
We have defined shared record types and DUs as well
For instance, ``BlockHandlers`` are expecting an input of ``string``and output ``Block list``.

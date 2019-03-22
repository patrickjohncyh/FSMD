## Personal Statement - ljo16

**Contributions:**
1. Wrote `tableHandler` which produces a DOM-like structure of a table block when given a string of table contents
2. Studied the Visual2 Renderer to determine how and where Monaco outputs the content of the editor which was important for the 3rd point
3. Worked with Patrick John to lay out the groundwork, to find a way to redirect Monaco output to our backend. Then taking the output of our backend (of type ```Block List```) and processing it into a HTML DOM document by
4. Writing most of ```blockListToDom``` function(contained in Integrations.fs), that takes as input, a ```Block List```(each ```Block``` having a DOM-like tree structure). I had to breakdown every ```Block``` into its most basic elements, convert those elements into ```HTMLElements```, then take the ```HTMLElements``` to build the entire HTML DOM document bottom-up.  I wrote almost all of the code in Integrations.fs
5. Once I was done with Integrations.fs, I had to go back to ```tableHandler``` to make quite a bit of a change to allow dynamic sizing of tables in markdown. This allows users to choose whether they want to declare the width and height of any table they make. This was done as promised inside my Individual stage README "future plans". 

**Challenges:**
1. As I did almost all of the conversion to DOM parts, I was required to understand Javascript which I had little to no knowledge of at the beginning. I was fascinated when I found out how Javascript edits HTML documents.
2. Writing Javascript in F#(through Fable) is not as seamless as it is seems. There were quite some hiccups during my wonderful adventure. For example, if you would like to add/edit the 'width' attribute of a HTML element such as 'th' in tables:
      1. In Javascript, that would just be HTMLElementObject.width = "something"
      2. In F#/Fable, width is not recognized in their most basic ```HTMLElement``` type. You will have to cast it into the right table type to access such attributes.
      3. There weren't a lot of information on Fable other than their documentation and there were times I had problems understanding their documentation. 
3. During the last 2 weeks, my teammates were struggling to find time to do this project given that we also have other module coursework deadlines. To make matters worse, when I was doing the DOM conversion parts, I found that we had to make changes to our backend(Individual phase) to accommodate the information required in HTML DOM. This impeded progress, and I had to inform my teammates of the changes required and set "internal deadlines" to make these changes, to ensure progress in the project.

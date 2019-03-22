# FSMD

For a complete reference of markdown specifications that this project is based, click [here](https://spec.commonmark.org/0.28/)  
For a quick start go straight to [Getting Started](#how-to-build).  
The rest of the README gives a project and code overview.

## Go To
* [Introduction](#introduction)
* [Module Structures](#module-structures)
* [How to Build](#how-to-build)
* [Features Implemented](#features-implemented)
* [Future Extensions](#future-works)

## Introduction

FSMD is an F# markdown implementation that converts markdown texts into HTML DOM (Document Object Model),  
which is then styled with CSS to produce interactive representations.

This project's front end is based on https://github.com/ImperialCollegeLondon/Visual2 Renderer code.

The target language is `F#`, which is transpiled to `Javascript` (`js`) thanks to [Fable](https://fable.io) v2.x. [Electron](https://electronjs.org/) is then used to convert the developed web-app to a cross-platform native application, providing access to platform-level commands (i.e. file-system, path, multiple processes), which are unavailable to (vanilla) browser web-apps.

[Webpack](https://webpack.js.org/) is the module bundler, responsible for the Javascript concatenation and automated building process.

Finally, [Monaco Editor](https://microsoft.github.io/monaco-editor/) is  a self-contained Javascript component that implements a programmer's editor window, with many features, which the `F#` code can interact with.

## Module Structures

### Backend
Overall backend structure is as seen below:
```

Overall Flowchart
                                                       ---> Output (Block list, DOM-like structure)
                                                       |
                                                       |
                       ________________       ____________________      __________________       _________________
     source       ---> | Block Parser | --->  | Block Dispatcher | ---> | Block Handlers | --->  | Inline Parser |
 (markdown text)       |______________|       |__________________|      |       &        |       |_______________|
                           ^                                            |  Table Handler |              
                           |                                            |________________|              
                           |                                                    |                       
                           |  in cases where blocks are recursive (i.e) list    |                       
                           |____________________________________________________|                       
```

`Block Parser` reads each line of markdown text and identifies the types of each line storing it in a list and sending it to  
`Block Dispatcher` which will look at the list and converts each entry into a data type that resembles a DOM  
`Inline Parser` parses symbols that are used in markdwon to style inline text element.

More in depth explanation about what each block does:
1. [Block Parser & Block Dispatcher](https://github.com/patrickjohncyh/FSMD/tree/hi116-indiv/)
2. [Block Handler](https://github.com/patrickjohncyh/FSMD/tree/origin/yhl116-origin/yeehonglow)
3. [Table Handler](https://github.com/patrickjohncyh/FSMD/tree/ljo16-indiv/)
4. [Inline Parser](https://github.com/patrickjohncyh/FSMD/tree/pjc316-indiv)


### Frontend
The frontend of FSMD is a stripped down version of VISUAL2.
Styling of markdown output follows GitHub markdown representation using the css styling by  
(https://github.com/sindresorhus/github-markdown-css/blob/gh-pages/github-markdown.css)


## How to build
The build instructions are more or less the same as [Visual2](https://github.com/ImperialCollegeLondon/Visual2)

1. Follow instructions to install [yarn](https://yarnpkg.com/lang/en/docs/install/) (which tell you to install Node as well).

2. Download and install the latest (2.x) [Dotnet Core SDK](https://www.microsoft.com/net/learn/get-started).  
For Mac users, download and install [Mono](http://www.mono-project.com/download/stable/).

3. Download & unzip the repo, or if contributing clone it locally, or fork it on github and then clone it locally.

4. Navigate to the project root directory.

5. Fetch the required `npm` packages by executing `yarn install`. This project consistently uses `yarn` Node package manager instead of `npm`.

6. On macOS or linux ensure you have [paket installed](https://fsprojects.github.io/Paket/installation.html). Run `setup.bat` (on Windows) or `sh setup.sh` (on linux or macOS).

7. In a terminal window (for example under `hyper`) compile `fsharp` code to `javascript` using `webpack` by executing `yarn start` (shortcut for `yarn run start`). This runs the `start` script defined in `package.json`. The `start` script  compiles everything once and then watches source files recompiling whenever any change, so it is normal run continuously throughout development. You will need to view the `yarn start` output throughout development since if compile fails the output makes this clear via red-colored text. Although Ionide will also provide error messages on code that does not compile it is possible to miss these when making quick changes.

8. Open your `electron` app in a new terminal tab by running `yarn launch`. This command will start the application and also _hot reload_ it whenever source files are recompiled, or CSS files changed. Therefore it normally also runs continuously through development. The total time from saving an updated F# source file to reload is typically 5s. Make sure you have this development environment working effectively for you: an HD screen without scaling is helpful because it allows your editor, the Visual2 app, and the command windows all to be visible simultaneously. Using *Hyper* `File->Split Horizontally is useful to run `lauch` and `start` concurrently.

9. To see console debug printout etc from the running FSMD app press `Ctrl-Shift-I` to toggle electron dev tools on and note that any F# printout and errors will be displayed under the console tab.

## Features Implemented

### Markdown Standard Features
Below is the list of standard features available, with some comments if they are slightly different from commonmark
* Lists
* Code Block ---> Only supports fenced code block \`
```
let x = a + b
```
* ##### Headings
* > BlockQuote
* Paragraph
* Links, Images
* Link Reference, Image Reference
* Inline Styling (**Bold**, *Itallic*, `inlinecode`, etc) for inline styling one must put parens () on the characters  
intended to be styled, for example for bold, \*\*(bolded characters)\*\* and itallic, it is \*(italicised characters)\*.

### Additional Implemented Features
Here are several additional features not part of markdown specification but is deemed useful:
* Tables               ---> Able to parse and style the table as well
Here is the way to make table, the height and width of the table can be adjusted from the first cell  
X and Y are the width and height of the tables respectively  
\| $$$x,y | Table Head 1 | Table Head 2 |  
\| ---- | Delimiter cell, divides table head and table body    
\| Table body |  

| Table Head 1 | Table Head 2 |
| ---- | ---- |
| Table body | Table body |

* Katex                ---> Part of inline styling, [KATEX](https://katex.org/) is latex-like typesetting library 
                            that allows equations to be represented with mathematical style. It is a js library that
                            must be added by either using `npm` or `yarn`
* Auto Parse           ---> Current markdown file under editing is automatically parsed every 5 seconds
* Fragment Identifier  ---> Able to jump to the part of the document referred to.
* Resizeable panes
* Tables with adjustable height and weight

## File Structure

### `src` folder

#### `src/FSMD`

The markdown source `F#` code. This is referenced as subproject, although under FABLE it is compiled uniformly with the renderer.

#### `src/Renderer`

The web-app GUI and Monaco editor interface source code.

#### `src/main`

Contains the F# source for the Electron startup main process code (mostly boilerplate).

### `app` folder

The web-app, view, startup files.

#### `app/index.html`

The markup code for the view.
`src/Renderer/Refs.fs` module accesses the elements defined in this DOM tree.

#### `app/css`

`CSS` code to prettify the `index.html` elements.

#### `app/js`

The `js` scripts loaded by the `index.html`, **after** the DOM elements (statically defined) are rendered.

##### `app/js/monaco-init.js`

`Monaco Editor` setup script, changes are done to support markdown grammar identification.

##### `app/js/vs`

This subdirectory is copied by webpack from ./node_modules/monaco-editor/min/vs.

It works around the fact that _packaging tools_ do not understand the non-standard Monaco loader, which loads Monaco editor files. Therefore to make things work the Monaco loader dependencies are all copied to the app directly in this directory. Note that extra code in the `webpack.config` script to allow this.

## Future Works
`HTML` tags are yet to be done, functionality to go to a specific part of the document if we are clicking on the generated markdown and text file vice versa.

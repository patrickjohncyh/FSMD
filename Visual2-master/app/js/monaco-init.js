/*
Visual2 @ Imperial College London
Project: A user - friendly ARM emulator in F# and Web Technologies(Github Electron & Fable Compliler)
Module: JS/Monaco-init
Description: Javascript code to run Monaco editor and define code highlighting regexes
*/


// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;

//
// get tippy.js node module for awesome tooltips
// global.tippy = require('tippy.js');
// NB this does not package properly:
// SOLUTION: use tippy-all-min.js
// add to app/js directory
// add <script src="js/tippy.all.min.js"></script> to index.html
// (optional, but best practice) to allow update via yarn, 
// copy this file from ./node_modules/tippy.js to app/js via webpack.config.js CopyWebpackPlugin
//


// todo: why does this work when tippy.js does not?
var path = require('path');

function uriFromPath(_path) {
  var pathName = path.resolve(_path).replace(/\\/g, '/');
  if (pathName.length > 0 && pathName.charAt(0) !== '/') {
    pathName = '/' + pathName;
  }
  return encodeURI('file://' + pathName);
}
amdRequire.config({
  //baseUrl: uriFromPath(path.join(__dirname, '../node_modules/monaco-editor/min'))
  baseUrl: uriFromPath(path.join(__dirname, 'js'))

});
// workaround monaco-css not understanding the environment
self.module = undefined;
// workaround monaco-typescript not understanding the environment
self.process.browser = true;
amdRequire(['vs/editor/editor.main'], function () {

  // Convert CSS-stle hex color (with #) to form needed by syntax highlighting.
  // Add a JS color display extension to have colors displayed in source
  function cs (color)  { 
    return color.substr(1);
  }

  var base03 = '#002b36';
  var base02 = '#073642';
  var base01 = '#586e75';
  var base00 = '#657b83';
  var base0 = '#839496';
  var base1 = '#93a1a1';
  var base2 = '#eee8d5';
  var base3 = '#fdf6e3';
  var yellow = '#b58900';
  var orange = '#cb4b16';
  var red = '#dc322f';
  var magenta = '#d33682';
  var violet = '#6c71c4';
  var blue = '#268bd2';
  var cyan = '#2aa198';
  var green = '#859900';

  monaco.editor.defineTheme('one-light-pro', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#303030')},
      { token: 'keyword', foreground: cs('#1010a0')},
      { token: 'symbols', foreground: cs('#303030')},
      { token: 'comment', foreground: cs('#308030')},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#ff0000')},
      { token: 'string', foreground: cs('#e06c75') },
      {token: 'number.bare', foreground: cs("#c08000") },
      { token: 'number.hash', foreground: cs("#408080")}
    ],
    "colors": {
      'editor.foreground': '#000000',
      'editor.background': '#EDF9FA',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base3
    }
  });

  monaco.editor.defineTheme('one-dark-pro', {
    base: 'vs-dark',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#b0b0b0')},
      { token: 'keyword', foreground: cs(blue)},
      { token: 'symbol', foreground: cs('#a0a0a0')},
      { token: 'comment', foreground: cs('#20a020')},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2')},
      { token: 'string', foreground: cs('#e06c75')},
      { token: 'number.hash', foreground: cs("#80c0c0")},
      {token: 'number.bare', foreground: cs("#f0f080")}
    ],
    "colors": {
      'editor.foreground': '#FFFFFF',
      //'editor.background': '#000000',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base02,
      'editorLineNumber.foreground': base01,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base01,
      'editor.findMatchBackground': base00, // Color of the current search match.
      'editor.findMatchHighlightBackground':base02 // Color of the other search matches.
    }
  });

  
  monaco.editor.defineTheme('solarised-light', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'delimiter', foreground: cs(base00)},
      { token: 'identifier', foreground: cs(base00)},
      { token: 'keyword', foreground: cs(blue)},
      { token: 'symbol', foreground: cs(base00)},
      { token: 'comment', foreground: cs(green)},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2')},
      { token: 'string', foreground: cs('#e06c75')},
      { token: 'number.hash', foreground: cs(cyan)},
      {token: 'number.bare', foreground: cs(yellow)}
    ],
    "colors": {
      'foreground': base00,
      'editor.foreground': base00,
      'editor.background': base3,
      'editorCursor.foreground': magenta,
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': base1,
      'editor.inactiveSelectionBackground': base1,
      'editor.findMatchBackground': base0, // Color of the current search match.
      'editor.findMatchHighlightBackground':base2 // Color of the other search matches.
    }
  });


    monaco.editor.defineTheme('solarised-dark', {
      base: 'vs-dark',
      inherit: true, // can also be false to completely replace the builtin rules
      rules: [
        { token: 'delimiter', foreground: cs(base0)},
        { token: 'identifier', foreground: cs(base0)},
        { token: 'keyword', foreground: cs(blue)},
        { token: 'symbol', foreground: cs(base0)},
        { token: 'comment', foreground: cs(green)},
        { token: 'comment.testerror', foreground: cs(red) },
        { token: 'escape', foreground: cs('#57b6c2')},
        { token: 'string', foreground: cs('#e06c75')},
        { token: 'number.hash', foreground: cs(cyan)},
        {token: 'number.bare', foreground: cs(yellow)}
      ],
      "colors": {
        'foreground': base0,
        'editor.foreground': base0,
        'editor.background': base03,
        'editorCursor.foreground': magenta,
        'editor.lineHighlightBackground': base02,
        'editorLineNumber.foreground': base01,
        'editor.selectionBackground': base01,
        'editor.inactiveSelectionBackground': base01,
        'editor.findMatchBackground': base00, // Color of the current search match.
        'editor.findMatchHighlightBackground':base02 // Color of the other search matches.
      }
    });
  
  var mevent = new CustomEvent("monaco-ready", { "detail": "ready now!" });

  // Dispatch/Trigger/Fire the event
  document.dispatchEvent(mevent);
});






# Todo App
### Basic TodoMVC in Elm

Demo project demonstrating basics of Elm app, like The Elm Architecture, styling, interop with JavaScript through ports.

Following libraries were used:
* elm-lang/html - for rendering UI elements
* styled-components/elm-styled - for styling UI elements with regular CSS
* Json.Encode/Decode - for turning Elm types into primitives JavaScript can understand, and vice versa (essential for interop)

To see this app in action on your local machine just clone the repo and open index.html file, located at it's root, with your favourite browser. For this you don't need to install Elm. 

If you want to play with code, first make sure you have Elm on your local machine. Shell will reply with man page, if you type 'elm' in it, in case you have Elm installed. Otherwise, get it from official website http://elm-lang.org/.
Every time you make change to Main.elm file, make sure to recompile it with ```elm-make Main.elm --output="main.js"```, so that index.html can pick up the latest updates and display things correctly.

### The Elm Architecture boilerplate
If you want to build something awesome from scratch, you can grab the skeleton boilerplate from this gist: https://gist.github.com/grigor-aramyan/b29a6272f5656d9ddc56f591a3e03063. It has the structure of all neccesary elements, so you just need to fill in as you wish. Or just play with it to understand some fundamental concepts of it's workings.

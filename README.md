# [BuckleFrog](https://samouri.github.com/bucklefrog)

[BuckleFrog](https://samouri.github.com/bucklefrog) is an HTML5 Canvas based implementation of the 1981 Konami classic Frogger game. It is written in OCaml and transpiled to JavaScript.

![gameplay gif](https://github.com/FriteGames/buckle-frogger/raw/master/gameplay.gif)

[Click here](https://samouri.github.com/bucklefrog) to play!

## Gamplay features

* A cute frog with a pleasant jumping animation
* A highway and river, both perfect for killing said frog
* High score tracking so you can get needlessly competitive with your friends

## Technical description

The code is written in OCaml and transpiled to JavaScript via [js_of_ocaml](https://ocsigen.org/js_of_ocaml/). Dune drives the build, producing a JavaScript bundle that can be loaded directly in `index.html`.

The game features a pretty standard gameloop in `index.ml`. The most remarkable part of the code is how state is updated on each frame. In order to use a functional approach (as opposed to a more standard object oriented one), state is updated via a reducer-like function with the signature: `val stepWorld: worldT -> int -> worldT`. `stepWorld` takes in the current world and the time elapsed since the last frame, then returns the next state of the world.

Within `stepWorld` there is a multi-pass architecture [similar to some compilers](https://en.wikipedia.org/wiki/Multi-pass_compiler). Each "pass" has responsibility for updating a single piece of state or generating some intermediate values. For example `updateHighScore` is an example of a pass that checks if the current score is greater than the high score and updates the highscore if necessary. Some passes don't update any final state and are only responsible for generating some intermediate data. An example of that would be `findCollisions`. Collision data is necessary for other passes to operate, but isn't actually necessary for rendering and so doesn't end up in `worldT`.

**modules**

* _index.ml_: Contains the gameloop and initialization code
* _render.ml_: All of the logic for rendering to the canvas.
* _types.ml_: All of the types used in the game
* _state.ml_: `stepWorld` and all of the logic for updating state.
* _utils.ml_: Things that I had no idea where else to put. e.g. a new binary operator for generating a range between two numbers.

## To Run

Install `ocaml`, `dune`, and `js_of_ocaml` via your preferred package manager (e.g. `opam install dune js_of_ocaml`). Then build the JavaScript bundle:

```
dune build src/index.bc.js
```

Serve the repository root (for example `python -m http.server 8000`) and open `http://localhost:8000/index.html`. The page loads the compiled bundle from `_build/default/src/index.bc.js`.

## To Develop

Run `dune build src/index.bc.js` to compile, or `dune build --watch src/index.bc.js` for an incremental rebuild loop. Refresh the browser page after the build finishes.

##

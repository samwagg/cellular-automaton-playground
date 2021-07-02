# Cellular Automaton Playground

A GUI for running Cellular Automaton (CA) simulations written in Clojure. Uses [quil](http://quil.info/)
for graphics.

## Who this is for

This is primarily just for me. I'm interested in artificial life and emergence and wrote this
program to help me learn and explore celular automata.

## Usage
### Prerequisites
- Java 11
- Clojure CLI
### Running with the Clojure CLI
`clj -M src/ca_playground/render.clj`
### Usage
You can cycle the state of individual cells by clicking them. Other operations are performed at
the keyboard. A list of commands are rendered at the bottom of the window.

Most operations can only be performed while the simulation is paused.

## Developing
### Running Tests
clj -X:test

### Extending
#### Adding new CA rules
There is a var in the `render` namespace called `ca-configs`. Just add a new entry to that
collection. For example, this is the entry for Game of Life:

```
{:name         "Game of Life"
 :cell-states  [{:color [255 255 255]}
                {:color [0 0 0]}]
 :rule-fn      ca/game-of-life-update-fn}
```

`:cell-states` is a vector whose length matches the number of distinct cell states in the
CA. `:color` is a vector representing an RGB color that will be used to fill each cell in that state
(it is currently the only key used for state maps).

`:rule-fn` is a 2-arity function that is invoked to update the state of each cell at each time step.
The first argument passed is the value of the cell to be updated and the second argument is the
cell's Moore Neighborhood represented as a flat seq where the neighboring cells are listed from left
to right and top to bottom. For example, a `Game of Life` grid may look like:

```
0 1 0 0 1
1 0 1 1 0
1 0 0 1 1
```

When `:rule-fn` is invoked for the centermost cell in the above grid, the arguments will be `1` and `(1 0 0 0 1 0 0 1)`.

## TODO
- More tests
- Performance optimizations (run larger grids with a reasonable frame rate).
- Add additional options to the UI, such adjusting grid size or frame rate.
- Add support for saving, replaying, rewinding, etc.

## License

Copyright © 2021 Sam Waggoner

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

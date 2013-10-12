Game of life
============

This is a functional implementation of Conway's Game of Life.

This was an exercise in embracing functional Scala for a problem that
is traditionally tackled using imperative approaches (keeping a grid
of bools and looping through it to update state).

## Design

- doesn't store bother to entire grid
- completely immutable
- supports an infinite grid (only limited by the memory required to maintain list of alive cells)
- also supports constrained, finite grids

## Usage

At the moment it is more of a library than a stand-alone
tool. However, it does include a basic CLI interface.

The simplest way to use it is via `sbt`'s `run` task. First make sure
you have sbt installed and then run something like the following:

    $ sbt "run examples/pulsar"

More generally,

    $ sbt "run <input file>"

The game engine will play out the provided world according to the game
of life's rules. If the game completes (the world stops changing), the
script will end on the final state.

Note that The first run will be slow whlie sbt compiles the project
and fetches its dependencies.

### Input files

You can provide a text file as input, which will be parsed into a game
universe. Some examples can be found in the examples directory. The
path to a file describing your game world should be provided to sbt's
run command.

To see the examples, you can run commands like the following:


    $ sbt "run examples/pulsar"
    $ sbt "run examples/gliders"
    $ sbt "run examples/diehard"

You should replace the example file path with your own file. The
syntax for the files sohuld be fairly obvious after looking at the
examples, but in summary, the file constists of:

1. intro lines (must not include the reserved, `+` char)
2. a `+` char to denote the top-left of the universe
3. optionally, another `+` symbol on the same line to mark the top-right
4. optionally, a `+` char on a subsequent line to mark the bottom-left

3 and 4 are optional, but if you provide one, you must provide
both. The key is that with additional `+`s provided you will get a
game of life board of fixed size, which is probably what you want. The
`+` characters thus mark out the corners (the bottom-right corner can
be inferred and as such doesn't need to be provided). Without these,
the game of life will play out on an "infinite" board, which will
dynamically resize itself to fit its contents.

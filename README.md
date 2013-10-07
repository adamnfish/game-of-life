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

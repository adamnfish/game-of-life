package com.adamnfish.gol

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.exceptions.TestFailedException
import com.adamnfish.gol.io.WorldParser
import org.scalatest.matchers.should.Matchers

class UniverseTest extends AnyFreeSpec with Matchers {
  val constrainedGol = FiniteUniverse(6, 6)

  "isAlive" - {
    val world: World = Map(Cell(1, 1) -> true, Cell(2, 2) -> false)

    "returns true" - {
      "for an alive cell" in {
        InfiniteUniverse.isAlive(Cell(1, 1), world) shouldEqual true
      }
    }

    "returns false" - {
      "for dead cell" in {
        InfiniteUniverse.isAlive(Cell(2, 2), world) shouldEqual false
      }
      "by default" in {
        InfiniteUniverse.isAlive(Cell(3, 3), world) shouldEqual false
      }
    }
  }

  "neighbours" - {
    "should return the neighbours for a given cell" in {
      assertCellSetsEqual(
        InfiniteUniverse.neighbours(Cell(1, 1)),
        Set(
          Cell(0, 0),
          Cell(1, 0),
          Cell(2, 0),
          Cell(0, 1),
          Cell(2, 1),
          Cell(0, 2),
          Cell(1, 2),
          Cell(2, 2)
        )
      )
    }

    "should not return the cell itself as a neighbour" in {
      InfiniteUniverse.neighbours(Cell(1, 1)) should not contain Cell(1, 1)
    }

    "in a constrained world" - {
      "should not return neighbours off the top of the world" in {
        assertCellSetsEqual(
          constrainedGol.neighbours(Cell(1, 0)),
          Set(
            Cell(0, 0),
            Cell(2, 0),
            Cell(0, 1),
            Cell(1, 1),
            Cell(2, 1)
          )
        )
      }
      "should not return neighbours off the bottom of the world" in {
        assertCellSetsEqual(
          constrainedGol.neighbours(Cell(1, 5)),
          Set(
            Cell(0, 4),
            Cell(1, 4),
            Cell(2, 4),
            Cell(0, 5),
            Cell(2, 5)
          )
        )
      }
      "should not return neighbours off the left of the world" in {
        assertCellSetsEqual(
          constrainedGol.neighbours(Cell(0, 2)),
          Set(
            Cell(0, 1),
            Cell(1, 1),
            Cell(1, 2),
            Cell(0, 3),
            Cell(1, 3)
          )
        )
      }
      "should not return neighbours off the right of the world" in {
        assertCellSetsEqual(
          constrainedGol.neighbours(Cell(5, 2)),
          Set(
            Cell(4, 1),
            Cell(5, 1),
            Cell(4, 2),
            Cell(4, 3),
            Cell(5, 3)
          )
        )
      }
    }
  }

  "isAliveNext" - {
    // rules from wikipedia
    "Any live cell with fewer than two live neighbours dies, as if caused by under-population" in {
      val world = WorldParser.fromCells(Cell(0, 0), Cell(0, 1), Cell(10, 10))
      InfiniteUniverse.isAliveNext(Cell(0, 1), world) shouldEqual false
      InfiniteUniverse.isAliveNext(Cell(10, 10), world) shouldEqual false
    }

    "Any live cell with two or three live neighbours lives on to the next generation" in {
      val world = WorldParser.fromCells(
        Cell(0, 0),
        Cell(0, 1),
        Cell(1, 0),
        Cell(10, 10),
        Cell(10, 11),
        Cell(10, 12),
        Cell(11, 11)
      )
      InfiniteUniverse.isAliveNext(Cell(1, 0), world) shouldEqual true
      InfiniteUniverse.isAliveNext(Cell(11, 11), world) shouldEqual true
    }

    "Any live cell with more than three live neighbours dies, as if by overcrowding" in {
      val world = WorldParser.fromCells(
        Cell(0, 0),
        Cell(0, 1),
        Cell(0, 2),
        Cell(1, 0),
        Cell(1, 1)
      )
      InfiniteUniverse.isAliveNext(Cell(1, 1), world) shouldEqual false
    }

    "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction" in {
      val world = WorldParser.fromCells(Cell(0, 0), Cell(0, 1), Cell(0, 2))
      InfiniteUniverse.isAliveNext(Cell(1, 1), world) shouldEqual true
    }
  }

  "eligibleCells" - {
    "should return the neighbours of a cell" - {
      val world = WorldParser.fromCells(Cell(1, 1))

      "in a finite universe" in {
        val universe = FiniteUniverse(3, 3)
        universe.eligibleCells(world) shouldEqual Set(
          Cell(0, 0),
          Cell(0, 1),
          Cell(0, 2),
          Cell(1, 0),
          Cell(1, 2),
          Cell(2, 0),
          Cell(2, 1),
          Cell(2, 2)
        )
      }

      "in an infinite universe" in {
        InfiniteUniverse.eligibleCells(world) shouldEqual Set(
          Cell(0, 0),
          Cell(0, 1),
          Cell(0, 2),
          Cell(1, 0),
          Cell(1, 2),
          Cell(2, 0),
          Cell(2, 1),
          Cell(2, 2)
        )
      }

      "excluding cells if they are off the edge of a finite universe" in {
        val universe = FiniteUniverse(2, 2)
        universe.eligibleCells(world) shouldEqual Set(
          Cell(0, 0),
          Cell(0, 1),
          Cell(1, 0)
        )
      }
    }

    "should return empty if there are no alive cells" - {
      "in a finite universe" in {
        FiniteUniverse(10, 10).eligibleCells(
          WorldParser.fromCells()
        ) shouldEqual Set.empty
      }
      "even in an infinite universe" in {
        InfiniteUniverse.eligibleCells(
          WorldParser.fromCells()
        ) shouldEqual Set.empty
      }
    }
  }

  "constraints" - {
    val universe = FiniteUniverse(4, 3)
    "xMin" in {
      universe.constrainToXMin(-5) shouldEqual 0
    }
    "xMax" in {
      universe.constrainToXMax(10) shouldEqual 3
    }
    "yMin" in {
      universe.constrainToYMin(-5) shouldEqual 0
    }
    "yMax" in {
      universe.constrainToYMax(3) shouldEqual 2
    }

    "are not in effect in an infinite universe" in {
      InfiniteUniverse.constrainToXMin(-1000) shouldEqual -1000
      InfiniteUniverse.constrainToXMax(1000) shouldEqual 1000
      InfiniteUniverse.constrainToYMin(-1000) shouldEqual -1000
      InfiniteUniverse.constrainToYMax(1000) shouldEqual 1000
    }
  }

  "boundaries" - {
    "for constrained universe" - {
      "should be defined by the constraint" in {
        val universe = FiniteUniverse(3, 4)
        universe.minX(Map.empty) shouldEqual 0
        universe.minY(Map.empty) shouldEqual 0
        universe.maxX(Map.empty) shouldEqual 2
        universe.maxY(Map.empty) shouldEqual 3
      }
    }

    "for infinite universe" - {
      "should be defined by the edges of the active world" - {
        "should be defined by the constraint" - {
          "empty universe should have empty boundaries" in {
            InfiniteUniverse.minX(Map.empty) shouldEqual 0
            InfiniteUniverse.minY(Map.empty) shouldEqual 0
            InfiniteUniverse.maxX(Map.empty) shouldEqual 0
            InfiniteUniverse.maxY(Map.empty) shouldEqual 0
          }

          "populated world should define universe boundaries" in {
            val world = Map(Cell(-2, -3) -> true, Cell(1, 2) -> true)
            InfiniteUniverse.minX(world) shouldEqual -2
            InfiniteUniverse.minY(world) shouldEqual -3
            InfiniteUniverse.maxX(world) shouldEqual 1
            InfiniteUniverse.maxY(world) shouldEqual 2
          }

          "universe boundaries are not extended by dead cells in populated world" in {
            val world = Map(
              Cell(-2, -3) -> true,
              Cell(1, 2) -> true,
              Cell(100, 100) -> false
            )
            InfiniteUniverse.minX(world) shouldEqual -2
            InfiniteUniverse.minY(world) shouldEqual -3
            InfiniteUniverse.maxX(world) shouldEqual 1
            InfiniteUniverse.maxY(world) shouldEqual 2
          }
        }
      }
    }
  }

  def assertCellSetsEqual(a: Set[Cell], b: Set[Cell]): Unit = {
    if (a.size != b.size)
      throw new TestFailedException(
        s"Set sizes differ: ${a.toString()} has ${a.size} els, compared to ${b.size} in ${b.toString()}",
        20
      )
    if (a != b) {
      throw new TestFailedException(
        s"Set contents differ by ${a.diff(b)}: ${a.toString()} did not equal ${b.toString()}",
        20
      )
    }
  }
}

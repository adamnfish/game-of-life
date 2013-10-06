package com.adamnfish.gol

import org.scalatest.FreeSpec
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.ShouldMatchers

class GolTest extends FreeSpec with ShouldMatchers {
  val constrainedGol = ConstrainedGol(6, 6)

  "isAlive" - {
    val world: World = Map(Cell(1, 1) -> true, Cell(2, 2) -> false)

    "returns true" - {
      "for an alive cell" in {
        InfiniteGol.isAlive(Cell(1, 1), world) should equal(true)
      }
    }

    "returns false" - {
      "for dead cell" in {
        InfiniteGol.isAlive(Cell(2, 2), world) should equal(false)
      }
      "by default" in {
        InfiniteGol.isAlive(Cell(3, 3), world) should equal(false)
      }
    }
  }

  "neighbours" - {
    "should return the neighbours for a given cell" in {
      assertCellSetsEqual(InfiniteGol.neighbours(Cell(1, 1)), Set(
        Cell(0, 0),
        Cell(1, 0),
        Cell(2, 0),
        Cell(0, 1),
        Cell(2, 1),
        Cell(0, 2),
        Cell(1, 2),
        Cell(2, 2)
      ))
    }

    "should not return the cell itself as a neighbour" in {
      InfiniteGol.neighbours(Cell(1, 1)) should not contain Cell(1, 1)
    }

    "in a constrained world" - {
      "should not return neighbours off the top of the world" in {
        assertCellSetsEqual(constrainedGol.neighbours(Cell(1, 0)), Set(
          Cell(0, 0),
          Cell(2, 0),
          Cell(0, 1),
          Cell(1, 1),
          Cell(2, 1)
        ))
      }
      "should not return neighbours off the bottom of the world" in {
        assertCellSetsEqual(constrainedGol.neighbours(Cell(1, 5)), Set(
          Cell(0, 4),
          Cell(1, 4),
          Cell(2, 4),
          Cell(0, 5),
          Cell(2, 5)
        ))
      }
      "should not return neighbours off the left of the world" in {
        assertCellSetsEqual(constrainedGol.neighbours(Cell(0, 2)), Set(
          Cell(0, 1),
          Cell(1, 1),
          Cell(1, 2),
          Cell(0, 3),
          Cell(1, 3)
        ))
      }
      "should not return neighbours off the right of the world" in {
        assertCellSetsEqual(constrainedGol.neighbours(Cell(5, 2)), Set(
          Cell(4, 1),
          Cell(5, 1),
          Cell(4, 2),
          Cell(4, 3),
          Cell(5, 3)
        ))
      }
    }
  }

  "isAliveNext" - {
    // rules from wikipedia
    "Any live cell with fewer than two live neighbours dies, as if caused by under-population" in {

    }

  }

  "eligibleCells" - {

  }

  "boundaries" - {
    "for constrained universe" - {
      "should be defined by the constraint" in {
        val universe = ConstrainedGol(3, 4)
        universe.minX(Map.empty) should equal(0)
        universe.minY(Map.empty) should equal(0)
        universe.maxX(Map.empty) should equal(2)
        universe.maxY(Map.empty) should equal(3)
      }
    }

    "for infinite universe" - {
      "should be defined by the edges of the active world" - {
        "should be defined by the constraint" - {
          "empty universe should have empty boundaries" in {
            InfiniteGol.minX(Map.empty) should equal(0)
            InfiniteGol.minY(Map.empty) should equal(0)
            InfiniteGol.maxX(Map.empty) should equal(0)
            InfiniteGol.maxY(Map.empty) should equal(0)
          }

          "populated world should define universe boundaries" in {
            val world = Map(Cell(-2, -3) -> true, Cell(1, 2) -> true)
            InfiniteGol.minX(world) should equal(-2)
            InfiniteGol.minY(world) should equal(-3)
            InfiniteGol.maxX(world) should equal(1)
            InfiniteGol.maxY(world) should equal(2)
          }

          "universe boundaries are not extended by dead cells in populated world" in {
            val world = Map(Cell(-2, -3) -> true, Cell(1, 2) -> true, Cell(100, 100) -> false)
            InfiniteGol.minX(world) should equal(-2)
            InfiniteGol.minY(world) should equal(-3)
            InfiniteGol.maxX(world) should equal(1)
            InfiniteGol.maxY(world) should equal(2)
          }
        }
      }
    }
  }

  "nextWorld" - {
    "integration tests: " - {
      "#1, infinite" ignore {

      }

      "#2, constrained example" ignore {

      }
    }
  }

  def assertCellSetsEqual(a: Set[Cell], b: Set[Cell]): Unit = {
    if (a.size != b.size) throw new TestFailedException(s"Set sizes differ: ${a.toString()} has ${a.size} els, compared to ${b.size} in ${b.toString()}", 20)
    if (a != b) {
      throw new TestFailedException(s"Set contents differ by ${a.diff(b)}: ${a.toString()} did not equal ${b.toString()}", 20)
    }
  }
}

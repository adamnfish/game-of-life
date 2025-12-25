package com.adamnfish.gol.io

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import com.adamnfish.gol._
import com.adamnfish.gol.Cell

class WorldParserTest extends AnyFreeSpec with Matchers {
  "worldFromContent" - {
    "extracts a world as it should" in {
      val world = WorldParser.worldFromContent(List("+", "  x", " x"))
      assertWorld(FiniteUniverse(2, 2), world, Cell(1, 0), Cell(0, 1))
    }
  }

  "fromString" - {
    "extracts infinite universe from unconstrained string" in {
      val worldString =
        """
          |+---
          || x
          ||    x
          ||
        """.stripMargin
      val (_, universe) = WorldParser.fromString(worldString)
      universe shouldEqual InfiniteUniverse
    }

    "extracts the world from  unconstrained string" in {
      val worldString =
        """
          |+---
          || x
          ||    x
          ||
        """.stripMargin
      val (world, _) = WorldParser.fromString(worldString)
      assertWorld(InfiniteUniverse, world, Cell(1, 0), Cell(4, 1))
    }

    "can extract correctly constrained universe" in {
      val worldString =
        """+-----+
          || x
          ||x
          ||
          |+
        """.stripMargin
      val (world, universe) = WorldParser.fromString(worldString)
      universe.isInstanceOf[FiniteUniverse] shouldEqual true
      universe.maxX(world) shouldEqual 4
      universe.minX(world) shouldEqual 0
      universe.maxY(world) shouldEqual 2
      universe.minY(world) shouldEqual 0
    }

    "can parse constrained worlds" in {
      val world1 =
        """
          |+----+
          ||x x
          || x
          ||
          |+
        """.stripMargin
      val (world, universe) = WorldParser.fromString(world1)
      assertWorld(universe, world, Cell(0, 0), Cell(2, 0), Cell(1, 1))
    }
  }

  def assertWorld(universe: Universe, world: World, liveCells: Cell*): Unit = {
    (universe.minX(world) to universe.maxX(world)) flatMap { x =>
      (universe.minY(world) to universe.maxY(world)) map { y =>
        val cell: Cell = Cell(x, y)
        if (liveCells.contains(cell)) universe.isAlive(cell, world) shouldEqual true
        else universe.isAlive(cell, world) shouldEqual false
      }
    }
  }
}

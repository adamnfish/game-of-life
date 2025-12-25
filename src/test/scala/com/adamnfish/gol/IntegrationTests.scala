package com.adamnfish.gol

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import com.adamnfish.gol.io.WorldParser

class IntegrationTests extends AnyFunSuite with Matchers {
  test("Block should stay still") {
    val (world, universe) = WorldParser.fromStrings(
      "    ",
      " xx ",
      " xx ",
      "    "
    )
    universe.nextWorld(world) shouldEqual world
  }

  test("Beehive should stay still") {
    val (world, universe) = WorldParser.fromStrings(
      "      ",
      "  xx  ",
      " x  x ",
      "  xx  ",
      "      "
    )
    universe.nextWorld(world) shouldEqual world
  }

  test("Blinker should oscillate") {
    val (world1, universe) = WorldParser.fromStrings(
      "     ",
      "     ",
      " xxx ",
      "     ",
      "     "
    )
    val (world2, _) = WorldParser.fromStrings(
      "     ",
      "  x  ",
      "  x  ",
      "  x  ",
      "     "
    )
    universe.nextWorld(world1) shouldEqual world2
    universe.nextWorld(world2) shouldEqual world1
  }

  test("Beacon should oscillate") {
    val (world1, universe) = WorldParser.fromStrings(
      "      ",
      " xx   ",
      " xx   ",
      "   xx ",
      "   xx ",
      "      "
    )
    val (world2, _) = WorldParser.fromStrings(
      "      ",
      " xx   ",
      " x    ",
      "    x ",
      "   xx ",
      "      "
    )
    universe.nextWorld(world1) shouldEqual world2
    universe.nextWorld(world2) shouldEqual world1
  }

  test("glider should glide (4-step loop that translates by 1, 1)") {
    val (initialWorld, _) = WorldParser.fromStrings(
      "x   ",
      " xx ",
      "xx  ",
      "    "
    )
    val (expectedWorld2, _) = WorldParser.fromStrings(
      " x  ",
      "  x ",
      "xxx ",
      "    "
    )
    val (expectedWorld3, _) = WorldParser.fromStrings(
      "    ",
      "x x ",
      " xx ",
      " x  "
    )
    val (expectedWorld4, _) = WorldParser.fromStrings(
      "    ",
      "  x ",
      "x x ",
      " xx "
    )
    val universe = FiniteUniverse(6, 6)
    val world2 = universe.nextWorld(initialWorld)
    world2 shouldEqual expectedWorld2
    val world3 = universe.nextWorld(world2)
    world3 shouldEqual expectedWorld3
    val world4 = universe.nextWorld(world3)
    world4 shouldEqual expectedWorld4
    val world5 = universe.nextWorld(world4)
    world5 shouldEqual translate(1, 1, initialWorld)
    val world6 = universe.nextWorld(world5)
    world6 shouldEqual translate(1, 1, expectedWorld2)
    val world7 = universe.nextWorld(world6)
    world7 shouldEqual translate(1, 1, expectedWorld3)
    val world8 = universe.nextWorld(world7)
    world8 shouldEqual translate(1, 1, expectedWorld4)
  }

  def translate(x: Int, y: Int, world: World): World = {
    world.map { case ((cell, state)) =>
      Cell(cell.x + x, cell.y + y) -> state
    }.toMap
  }
}

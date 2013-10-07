package com.adamnfish.gol

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import com.adamnfish.gol.io.WorldParser

class IntegrationTests extends FunSuite with ShouldMatchers {

  test("Block should stay still") {
    val (world, universe) = WorldParser.fromStrings(
      "    ",
      " xx ",
      " xx ",
      "    "
    )
    universe.nextWorld(world) should equal(world)
  }

  test("Beehive should stay still") {
    val (world, universe) = WorldParser.fromStrings(
      "      ",
      "  xx  ",
      " x  x ",
      "  xx  ",
      "      "
    )
    universe.nextWorld(world) should equal(world)
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
    universe.nextWorld(world1) should equal(world2)
    universe.nextWorld(world2) should equal(world1)
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
    universe.nextWorld(world1) should equal(world2)
    universe.nextWorld(world2) should equal(world1)
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
    world2 should equal(expectedWorld2)
    val world3 = universe.nextWorld(world2)
    world3 should equal(expectedWorld3)
    val world4 = universe.nextWorld(world3)
    world4 should equal(expectedWorld4)
    val world5 = universe.nextWorld(world4)
    world5 should equal(translate(1, 1, initialWorld))
    val world6 = universe.nextWorld(world5)
    world6 should equal(translate(1, 1, expectedWorld2))
    val world7 = universe.nextWorld(world6)
    world7 should equal(translate(1, 1, expectedWorld3))
    val world8 = universe.nextWorld(world7)
    world8 should equal(translate(1, 1, expectedWorld4))
  }

  def translate(x: Int, y: Int, world: World): World = {
    world.map { case ((cell, state)) =>
      Cell(cell.x + x, cell.y + y) -> state
    }.toMap
  }
}

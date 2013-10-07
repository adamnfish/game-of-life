package com.adamnfish.gol

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

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
}

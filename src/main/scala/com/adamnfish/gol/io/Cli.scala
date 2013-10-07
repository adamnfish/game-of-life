package com.adamnfish.gol.io

import com.adamnfish.gol.{FiniteUniverse, InfiniteUniverse, World}

object Cli extends App {
  val contents = io.Source.fromFile(args(0)).mkString
  val (world, universe) = WorldParser.fromString(contents)
  evolve(world)

  def evolve(world: World): World = {
    Thread.sleep(300)
    clearScreen()
    WorldPrinter.print(world, universe) foreach println
    evolve(universe.nextWorld(world))
  }

  def clearScreen() = print("\033[H\033[2J")
}

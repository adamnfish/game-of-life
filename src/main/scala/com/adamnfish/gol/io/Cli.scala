package com.adamnfish.gol.io

import com.adamnfish.gol.World

import scala.annotation.tailrec

object Cli extends App {
  val contents = io.Source.fromFile(args(0)).mkString
  val (world, universe) = WorldParser.fromString(contents)
  evolve(world)

  @tailrec
  def evolve(world: World): World = {
    Thread.sleep(300)
    clearScreen()
    WorldPrinter.print(world, universe) foreach println
    val next = universe.nextWorld(world)
    if (next == world) next
    else evolve(next)
  }

  def clearScreen() = print("\u001b[H\u001b[2J")
}

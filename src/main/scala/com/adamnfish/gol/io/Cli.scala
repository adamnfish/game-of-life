package com.adamnfish.gol.io

import com.adamnfish.gol.{Universe, World}

import scala.annotation.tailrec
import scala.util.Using

object Cli {
  @main
  def main(fileName: String): Unit = {
    val contents = Using.resource(io.Source.fromFile(fileName))(_.mkString)
    val (world, universe) = WorldParser.fromString(contents)
    enterAlternateScreen()
    try {
      evolve(world, universe)
    } finally {
      exitAlternateScreen()
    }
  }

  @tailrec
  def evolve(world: World, universe: Universe): World = {
    Thread.sleep(300)
    moveCursorHome()
    WorldPrinter.print(world, universe) foreach { line =>
      println(line + "\u001b[K") // Clear to end of line
    }
    clearToEnd() // Clear any leftover content from previous (larger) frames
    val next = universe.nextWorld(world)
    if (next == world) next
    else evolve(next, universe)
  }

  // Switch to another screen buffer so we don't pollute the scroll history
  def enterAlternateScreen(): Unit = print(
    "\u001b[?1049h\u001b[?25l\u001b[2J\u001b[H"
  )
  // Return to the previous terminal screen buffer
  def exitAlternateScreen(): Unit = print("\u001b[?25h\u001b[?1049l")
  // Move cursor to top-left of the terminal screen
  def moveCursorHome(): Unit = print("\u001b[H")
  // Clear from cursor to end of screen (removes everything previously displayed)
  def clearToEnd(): Unit = print("\u001b[J")
}

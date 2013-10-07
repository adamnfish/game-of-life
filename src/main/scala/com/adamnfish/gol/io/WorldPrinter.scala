package com.adamnfish.gol.io

import com.adamnfish.gol._

object WorldPrinter {
  def print(world: World): List[String] = print(world, Universe.universeForWorld(world))

  def print(world: World, universe: Universe): List[String] = {
    val content = (universe.minY(world) to universe.maxY(world)).toList map { y =>
      (universe.minX(world) to universe.maxX(world)) map { x =>
        if (universe.isAlive(Cell(x, y), world)) 'x'
        else ' '
      }
    }

    if (universe == InfiniteUniverse) {
      content.map(_.mkString).toList
    } else {
      val headerFooter = "+" + ("=" * content.head.size) + "+"
      headerFooter ::
            content.map("|" + _.mkString + "|") ::: List(headerFooter)
    }
  }
}

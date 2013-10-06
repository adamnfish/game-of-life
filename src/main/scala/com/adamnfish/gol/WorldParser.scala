package com.adamnfish.gol

object WorldParser {
  def fromString(input: String): (Gol, World) = ???

  def fromCells(cells: Cell*): World = {
    cells.map { (_, true) }.toMap
  }
}

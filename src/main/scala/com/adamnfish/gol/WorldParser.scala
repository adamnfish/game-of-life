package com.adamnfish.gol

object WorldParser {

  /*
   * Takes a multi-line string describing the world to be
   *
   * - It is started by a + symbol that defines 0, 0
   * - the row and column in line with the + are ignored
   * - any non-whitespace character down and right of the start + become live cells
   * - if the border row and column contain a +, a constrained universe is returned
   *
   * Note that it is not possible to place cells at negative positions
   * (although you could map the result to perform a translation)
   *
   * e.g.
   * Will produce an infinite universe and a world with these 6 cells living (these are equivalent):
   * +-----------     +                + - - - - - - - - -
   * | x                $              . 1
   * |    x x              $ $         .    2 3
   * |  x                $              4
   * |          x                $                5
   * |      x                $                6
   *
   * Will produce: a finite universe with 4 living cell (these are equivalent):
   * +========+    +--------+    +        +    +        +
   * |   x         |   a    |        o             .
   * | x     x     | b     c|    | o     o       .     .
   * |      x      |      d |           o             .
   * +             +--------+    +             +        +
   *
   * The tests will provide additional examples
   */
  def fromString(input: String): (World, Universe) = {
    val lines = input.split("\\r?\\n")
      .dropWhile(!_.contains('+'))  // drop leading lines
      .toList
    if (lines.isEmpty) throw new IllegalArgumentException("Input string did not contain starting + marker")
    if (lines.head.count('+' ==) > 1) {
      // finite, remove the line starts, trailing line bits after + and final lines after closing +
      val content = dropLineEnds(dropLineStarts(lines)) match {
        case startRow :: tail => startRow :: tail.takeWhile(_.head != '+')
      }
      (worldFromContent(content), FiniteUniverse(content.head.size - 1, content.size - 1))
    } else {
      // infinite, just drop line starts
      val content = dropLineStarts(lines)
      (worldFromContent(content), InfiniteUniverse)
    }
  }

  // removes leading noise on lines before col containing starting +
  private def dropLineStarts(lines: List[String]): List[String] = {
    val xStart = lines.head.indexWhere('+' ==)
    lines.map(_.drop(xStart))
  }

  // removes trailing noise on lines after closing +
  // You must have first run dropLineStarts
  private def dropLineEnds(lines: List[String]): List[String] = {
    assert('+' == lines(0)(0), "You must call dropLineStarts prior to dropLineEnds")
    val xEnd = lines.head.drop(1).indexWhere('+' ==) + 1
    lines.map(_.take(xEnd))
  }

  def worldFromContent(lines: List[String]): World = {
    lines.drop(1)  // remove first row
      .zipWithIndex.flatMap { case ((line, y)) =>
        line.drop(1)  // remove first col
          .zipWithIndex.flatMap { case ((char, x)) =>
            if (char != ' ') Some(Cell(x, y) -> true)
            else None
          }
      }.toMap
  }

  /*
   * Takes a list of cells, which are assumed to be alive
   */
  def fromCells(cells: Cell*): World = {
    cells.map { (_, true) }.toMap
  }
}

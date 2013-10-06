package com.adamnfish.gol

trait Gol {
  // playing area constraints
  val min: Option[Cell]
  val max: Option[Cell]

  /*
   * Returns a new world for the next "tick"
   */
  def nextWorld(world: World): World = {
    // TODO convert to set before getting state of neighbours to eliminate repetition
    eligibleCells(world).flatMap {
      case cell => neighbours(cell).flatMap { neighbour =>
        if (isAliveNext(neighbour, world)) Some((neighbour, true))
        else None
      }
    }.toMap
  }

  /*
   * List of coords for cells that might have a change
   */
  def eligibleCells(world: World): Set[Cell] = world.flatMap {
    case (cell, true) => neighbours(cell)
    case _ => Nil
  }.toSet

  /*
   * Gets the locations for neighbouring cells
   */
  def neighbours(cell: Cell): Set[Cell] = {
    (constrainToXMin(cell.x - 1) to constrainToXMax(cell.x + 1)).flatMap { x =>
      (constrainToYMin(cell.y - 1) to constrainToYMax(cell.y + 1)).flatMap { y =>
        if (x == cell.x && y == cell.y) None
        else Some(Cell(x, y))
      }
    }.toSet
  }

  /**
   * Returns a boolean describing cell's state in the provided world
   */
  def isAlive(cell: Cell, world: World): Boolean = world.get(cell).getOrElse(false)

  /*
   * Returns the state of this cell in the next tick by inspecting neighbours
   */
  def isAliveNext(cell: Cell, world: World): Boolean = {
    neighbours(cell).count(neighbour => isAlive(neighbour, world)) match {
      case x if x < 2 => false
      case 2 => isAlive(cell, world)
      case 3 => true
      case x if x > 3 => false
    }
  }

  /*
   * These functions provide boundaries that a UI may wish to respect
   */
  def minX: World => Int = extractBoundary(min, _.x, _.min)
  def maxX: World => Int = extractBoundary(max, _.x, _.max)
  def minY: World => Int = extractBoundary(min, _.y, _.min)
  def maxY: World => Int = extractBoundary(max, _.y, _.max)
  private def extractBoundary(limit: Option[Cell], xOrY: Cell => Int, minOrMax: Iterable[Int] => Int)(world: World): Int = {
    limit.map(xOrY).getOrElse {
      world.filter(_._2).map {
        case (cell, _) => xOrY(cell)
      } match {
        case Nil => 0
        case xs => minOrMax(xs)
      }
    }
  }

  /*
   * Internal functions to keep track of boundaries
   */
  // TODO: this is way overcomplicated! (Int, Option[Int]) => Int, surely
  private def minimum(ns: Int*) = ns.min
  private def maximum(ns: Int*) = ns.max
  private def constrainToXMin(n: Int) = min.map(minCell => maximum(n, minCell.x)).getOrElse(n)
  private def constrainToXMax(n: Int) = max.map(maxCell => minimum(n, maxCell.x)).getOrElse(n)
  private def constrainToYMin(n: Int) = min.map(minCell => maximum(n, minCell.y)).getOrElse(n)
  private def constrainToYMax(n: Int) = max.map(maxCell => minimum(n, maxCell.y)).getOrElse(n)

}
object InfiniteGol extends Gol {
  override val min = None
  override val max = None
}
class ConstrainedGol private(minCell: Cell, maxCell: Cell) extends Gol {
  override val min = Some(minCell)
  override val max = Some(maxCell)
}
object ConstrainedGol {
  def apply(width: Int, height: Int) = new ConstrainedGol(Cell(0, 0), Cell(width - 1, height - 1))
}

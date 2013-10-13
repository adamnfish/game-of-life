package com.adamnfish.gol

trait Universe extends Boundaries {
  /*
   * Returns a new world for the next "tick"
   */
  def nextWorld(world: World): World = {
    eligibleCells(world).flatMap {
      case cell => neighbours(cell)
    }.flatMap { neighbour =>
      if (isAliveNext(neighbour, world)) Some(neighbour -> true)
      else None
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
}
object Universe {
  /*
   * Returns a finite universe that will fit this world, suitable for UI display
   */
  def universeForWorld(world: World): FiniteUniverse = {
    FiniteUniverse(InfiniteUniverse.maxX(world) + 1, InfiniteUniverse.maxY(world) + 1)
  }
}

object InfiniteUniverse extends Universe with Unbounded

class FiniteUniverse private(minCell: Cell, maxCell: Cell) extends Universe with Bounded {
  override val min = minCell
  override val max = maxCell
}
object FiniteUniverse {
  def apply(width: Int, height: Int) = new FiniteUniverse(Cell(0, 0), Cell(width - 1, height - 1))
}
// TODO: ToroidalUniverse

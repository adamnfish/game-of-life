object Gol extends App {
  class Cell(val x: Int, val y: Int)
  type World = Map[Cell, Boolean]

  // if you were inclined to limit the world size, introduce these in neighbours method
//  val min: Option[Cell] = None
//  val max: Option[Cell] = None

  def isAlive(cell: Cell, world: World): Boolean = world.get(cell).getOrElse(false)

  // replace with a way of setting initial state
  val initialState: World = Map.empty

  /*
   * Returns a new world for the next "tick"
   */
  def nextState(world: World): World = {
    world.flatMap {
      case (cell, state) => neighbours(cell).map { neighbour =>
        (neighbour, nextState(neighbour, world))
      }
    }
  }

  /*
   * List of coords for cells that might have a change
   */
  def eligibleCells(world: World): Set[Cell] = world.flatMap {
    case (cell, true) => neighbours(cell)
  }.toSet

  /*
   * Gets the locations for neighbouring cells
   */
  def neighbours(cell: Cell): Set[Cell] = {
    ((cell.x - 1) to (cell.x + 1)).flatMap { x =>
      ((cell.y - 1) to (cell.y + 1)).flatMap { y =>
        if (x != cell.x && y != cell.y) Some(new Cell(x, y))
        else None
      }
    }.toSet
  }

  /*
   * Returns the state of this cell in the next tick by inspecting neighbours
   */
  def nextState(cell: Cell, world: World): Boolean = {
    neighbours(cell).count(neighbour => isAlive(neighbour, world)) match {
      case x if x < 2 => false
      case 2 => isAlive(cell, world)
      case 3 => true
      case x if x > 3 => false
    }
  }
}

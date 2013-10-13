package com.adamnfish.gol

trait Boundaries {
  /*
   * These functions provide boundaries that a UI may wish to respect
   */
  def minX(world: World): Int
  def maxX(world: World): Int
  def minY(world: World): Int
  def maxY(world: World): Int

  /*
   * Internal functions to keep track of boundaries
   */
  def constrainToXMin(n: Int): Int
  def constrainToXMax(n: Int): Int
  def constrainToYMin(n: Int): Int
  def constrainToYMax(n: Int): Int
}

trait Unbounded extends Boundaries {
  override def minX(world: World): Int = extractBoundary(_.x, _.min, world)
  override def maxX(world: World): Int = extractBoundary(_.x, _.max, world)
  override def minY(world: World): Int = extractBoundary(_.y, _.min, world)
  override def maxY(world: World): Int = extractBoundary(_.y, _.max, world)
  private def extractBoundary(xOrY: Cell => Int, minOrMax: Iterable[Int] => Int, world: World): Int = {
    world.filter(_._2).map {
      case (cell, _) => xOrY(cell)
    } match {
      case Nil => 0
      case xs => minOrMax(xs)
    }
  }

  override def constrainToXMin(n: Int): Int = n
  override def constrainToXMax(n: Int): Int = n
  override def constrainToYMin(n: Int): Int = n
  override def constrainToYMax(n: Int): Int = n
}

trait Bounded extends Boundaries {
  val min: Cell
  val max: Cell

  override def minX(world: World): Int = min.x
  override def maxX(world: World): Int = max.x
  override def minY(world: World): Int = min.y
  override def maxY(world: World): Int = max.y

  override def constrainToXMin(n: Int): Int = n max min.x
  override def constrainToXMax(n: Int): Int = n min max.x
  override def constrainToYMin(n: Int): Int = n max min.y
  override def constrainToYMax(n: Int): Int = n min max.y
}
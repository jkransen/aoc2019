package nl.kransen.aoc2019

import scala.annotation.tailrec
import scala.io.Source
import java.lang.Math._

object Day3 extends App {

  type Direction = Char

  case class Point(x: Int, y: Int, numSteps: Int) {
    lazy val distance: Int = abs(x) + abs(y)
    def samePoint(other: Point): Boolean = x == other.x && y == other.y
  }

  case class Trail(pointer: Point, visited: Set[Point]) {
    def this() = this(Point(0, 0, 0), Set())
  }

  case class Move private (direction: Direction, steps: Int) {
    def this(str: String) = this(direction = str.head, steps = str.tail.toInt)
  }

  def nextPoint(pointer: Point, direction: Direction): Point = {
    direction match {
      case 'U' => pointer.copy(y = pointer.y + 1, numSteps = pointer.numSteps + 1)
      case 'D' => pointer.copy(y = pointer.y - 1, numSteps = pointer.numSteps + 1)
      case 'L' => pointer.copy(x = pointer.x - 1, numSteps = pointer.numSteps + 1)
      case 'R' => pointer.copy(x = pointer.x + 1, numSteps = pointer.numSteps + 1)
    }
  }

  @tailrec
  def addCoordinates(trail: Trail, move: Move): Trail = {
    if (move.steps == 0) {
      trail
    } else {
      val nextPointer = nextPoint(trail.pointer, move.direction)
      val newVisited = if (trail.visited.exists(nextPointer.samePoint)) trail.visited else trail.visited + nextPointer
      addCoordinates(trail.copy(nextPointer, newVisited), move.copy(steps = move.steps - 1))
    }
  }

  def findCrossings(visited1: Set[Point], visited2: Set[Point]): Set[Point] = {
    for {
      first <- visited1
      second <- visited2
      if first.samePoint(second)
    } yield first.copy(numSteps = first.numSteps + second.numSteps)
  }

  def getMoves(line: String): List[Move] = {
    line.split(",").toList.map(new Move(_))
  }

  def trail(moves: List[Move]): Trail = {
    moves.foldLeft(new Trail()) {
      (trail, nextMove) => addCoordinates(trail, nextMove)
    }
  }

  def initMoves(): List[List[Move]] = {
    val source = Source.fromResource("day3/input.txt")
    val lines: List[String] = source.getLines().toList
    val moveses: List[List[Move]] = lines.map(getMoves)
    source.close()
    moveses.foreach(moves => println(s"${moves.size} moves: $moves"))
    moveses
  }

  val trails = initMoves().map(trail)
  val visited = trails.map(_.visited)
  visited.foreach(v => println(s"Trail containing ${v.size} points"))
  val crossings = visited.reduce(findCrossings)
  println(s"${crossings.size} crossings: $crossings")
  val sortByNearest: Ordering[Point] = (one, two) => one.distance - two.distance
  lazy val nearestCrossing = crossings.toList.sorted(sortByNearest).head
  println(s"${nearestCrossing} is nearest crossing at Manhattan distance ${abs(nearestCrossing.x + nearestCrossing.y)}")
  val sortByLeastCombinedSteps: Ordering[Point] = (one, two) => one.numSteps - two.numSteps
  lazy val leastCombinedSteps = crossings.toList.sorted(sortByLeastCombinedSteps).head
  println(s"${leastCombinedSteps} is crossing with least combined steps: ${leastCombinedSteps.numSteps}")
}

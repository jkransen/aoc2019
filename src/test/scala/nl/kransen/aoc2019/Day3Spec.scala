package nl.kransen.aoc2019

import nl.kransen.aoc2019.Day3.{Move, Point}
import org.scalatest._

class Day3Spec extends FlatSpec with Matchers {
  implicit val ord: Ordering[Point] = (one, two) => {
      one.numSteps - two.numSteps
  }

  "The first moves" should "be parsed correctly" in {
    val path = "U7,R6,D4,L4"
    val expected = List(Move('U', 7),Move('R', 6),Move('D', 4),Move('L', 4))
    Day3.getMoves(path) shouldEqual expected
  }

  "The visited points" should "contain intermediate points" in {
    val path = "U7,R6,D4,L4"
    val moves = Day3.getMoves(path)
    val trail = Day3.trail(moves).visited.toList.sorted
    trail shouldEqual List(Point(0,1,1), Point(0,2,2), Point(0,3,3), Point(0,4,4), Point(0,5,5), Point(0,6,6), Point(0,7,7),
      Point(1,7,8), Point(2,7,9), Point(3,7,10), Point(4,7,11), Point(5,7,12), Point(6,7,13), Point(6,6,14), Point(6,5,15),
      Point(6,4,16), Point(6,3,17), Point(5,3,18), Point(4,3,19), Point(3,3,20), Point(2,3,21))
  }

  "Part 2" should "finish with correct result" in {
    Day2.resultsPart2.headOption shouldEqual Some(4019)
  }

}

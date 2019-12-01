package nl.kransen.aoc2019

import org.scalatest._

class Day1Spec extends FlatSpec with Matchers {
  "The fuel required" should "for mass 12" in {
    Day1.fuelRequired(12) shouldEqual 2
  }

  "The fuel required" should "for mass 14" in {
    Day1.fuelRequired(14) shouldEqual 2
  }

  "The fuel required" should "for mass 1969" in {
    Day1.fuelRequired(1969) shouldEqual 654
  }

  "The fuel required" should "for mass 100756" in {
    Day1.fuelRequired(100756) shouldEqual 33583
  }

  "The fuel required" should "for all stars" in {
    val source = scala.io.Source.fromResource("day1/input.txt")
    val lines: List[Long] = source.getLines().map(_.toLong).toList
    val grossFuel = lines.fold(0L)((acc, nxt) => acc + Day1.fuelRequired(nxt))
    grossFuel shouldEqual 3167282
  }

  "The net fuel required" should "for mass 1969" in {
    Day1.netFuelRequired(1969) shouldEqual 966
  }

  "The net fuel required" should "for mass 100756" in {
    Day1.netFuelRequired(100756) shouldEqual 50346
  }

  "The net fuel required" should "for all stars" in {
    val source = scala.io.Source.fromResource("day1/input.txt")
    val lines: List[Long] = source.getLines().map(_.toLong).toList
    val grossFuel = lines.fold(0L)((acc, nxt) => acc + Day1.netFuelRequired(nxt))
    grossFuel shouldEqual 4748063
  }
}

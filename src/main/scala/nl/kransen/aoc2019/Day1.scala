package nl.kransen.aoc2019

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {
  def fuelRequired(mass: Long): Long = {
    (mass / 3).longValue - 2
  }

  def netFuelRequired(mass: Long): Long = {
    @tailrec
    def recurse(mass: Long, cumulative: Long = 0): Long = {
      val fuelReq = fuelRequired(mass)
      if (fuelReq > 0) {
        recurse(fuelReq, cumulative + fuelReq)
      } else {
        cumulative
      }
    }
    recurse(mass)
  }

  val source = Source.fromResource("day1/input.txt")
  val lines: List[Long] = source.getLines().map(_.toLong).toList
  source.close()

  val grossFuel = lines.fold(0L)((acc, nxt) => acc + fuelRequired(nxt))
  println(s"Gross fuel required: $grossFuel")
  val netFuel = lines.fold(0L)((acc, nxt) => acc + netFuelRequired(nxt))
  println(s"Net fuel required: $netFuel")
}

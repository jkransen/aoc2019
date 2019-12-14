package nl.kransen.aoc2019

import org.scalatest._

class Day2Spec extends FlatSpec with Matchers {
  "The example from text" should "have output 3500" in {
    val initial = Array(1,9,10,3,2,3,11,0,99,30,40,50)
    val expected = Array(3500,9,10,70,2,3,11,0,99,30,40,50)
    Day2.run(initial) shouldEqual expected
  }

  "Part 1" should "finish with correct result" in {
    Day2.run(Day2.initState())(0) shouldEqual 6627023
  }

  "Part 2" should "finish with correct result" in {
    Day2.resultsPart2.headOption shouldEqual Some(4019)
  }

}

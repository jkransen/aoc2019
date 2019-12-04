package nl.kransen.aoc2019

import org.scalatest._

class Day4Spec extends FlatSpec with Matchers {
  "hasDecrease" should "only match on decreasing digits" in {
    Day4.hasDecrease(List(1, 1, 1)) shouldEqual false
    Day4.hasDecrease(List(4, 5, 6)) shouldEqual false
    Day4.hasDecrease(List(4, 5, 3)) shouldEqual true
  }

  "hasDuplicate" should "only match duplicates" in {
    Day4.hasDuplicate(List(4, 5, 4)) shouldEqual false
    Day4.hasDuplicate(List(4, 5, 5)) shouldEqual true
    Day4.hasDuplicate(List(4, 5, 6)) shouldEqual false
  }

  "hasExactDuplicate" should "only match if a pair of exactly 2 is found" in {
    Day4.hasExactDuplicate(List(4, 5, 5)) shouldEqual true
    Day4.hasExactDuplicate(List(4, 4, 5, 5, 5)) shouldEqual true
    Day4.hasExactDuplicate(List(4, 4, 4, 5, 5, 5)) shouldEqual false
  }

  "number of duplicates" should "be 1640" in {
    Day4.duplicates.size shouldBe 1640
  }

  "number of exact duplicates" should "be 1126" in {
    Day4.exactDuplicates.size shouldBe 1126
  }
}

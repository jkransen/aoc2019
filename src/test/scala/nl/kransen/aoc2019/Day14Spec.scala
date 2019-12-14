package nl.kransen.aoc2019

import nl.kransen.aoc2019.Day14.{Ingredient, Recipe}
import org.scalatest._

class Day14Spec extends FlatSpec with Matchers {

  "Example 1" should "finish with correct result" in {
    val input = """
      |10 ORE => 10 A
      |1 ORE => 1 B
      |7 A, 1 B => 1 C
      |7 A, 1 C => 1 D
      |7 A, 1 D => 1 E
      |7 A, 1 E => 1 FUEL
      |""".stripMargin.linesIterator.toList.tail
    val recipes = Day14.toRecipes(input)
    recipes shouldEqual Map("E" -> Recipe(Ingredient("E",1),Set(Ingredient("A",7), Ingredient("D",1))), "A" -> Recipe(Ingredient("A",10),Set(Ingredient("ORE",10))), "FUEL" -> Recipe(Ingredient("FUEL",1),Set(Ingredient("A",7), Ingredient("E",1))), "B" -> Recipe(Ingredient("B",1),Set(Ingredient("ORE",1))), "C" -> Recipe(Ingredient("C",1),Set(Ingredient("A",7), Ingredient("B",1))), "D" -> Recipe(Ingredient("D",1),Set(Ingredient("A",7), Ingredient("C",1))))
    Day14.source(Set(Ingredient("FUEL", 1)), recipes) shouldEqual Set(Ingredient("ORE",31))
  }

  "Part 1" should "return the amount of ORE needed to produce 1 FUEL" in {
    Day14.result1.head.quantity shouldBe 483766
    Day14.oreForFuel(1) shouldBe 483766
  }

  "Part 2" should "return the greatest amount of FUEL needing < 1 trillion ORE" in {
    val ore = 1000000000000L
    ore / Day14.result1.head.quantity shouldBe 2067115

    Day14.oreForFuel(11759443)   shouldBe 999999992087L
    //                          val ore = 1000000000000L
    Day14.oreForFuel(11759444)   shouldBe 1000000063895L

    val i = for {
      i <- LazyList.from(11758000, 1)
      if Day14.oreForFuel(i) > 1000000000000L
    } yield i - 1
    println(i.head)
    i.head shouldBe 11759443
  }
}

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
    Day14.source(Set(Ingredient("FUEL", 1)), recipes) shouldEqual Set(Ingredient("ORE", 31))
  }

  "Example 2" should "finish with correct result" in {
    val input =
      """
        |9 ORE => 2 A
        |8 ORE => 3 B
        |7 ORE => 5 C
        |3 A, 4 B => 1 AB
        |5 B, 7 C => 1 BC
        |4 C, 1 A => 1 CA
        |2 AB, 3 BC, 4 CA => 1 FUEL
        |""".stripMargin.linesIterator.toList.tail
    val recipes = Day14.toRecipes(input)
    Day14.source(Set(Ingredient("FUEL", 1)), recipes) shouldEqual Set(Ingredient("ORE", 165))
  }

  "Example 3" should "finish with correct result" in {
    val input =
      """
        |157 ORE => 5 NZVS
        |165 ORE => 6 DCFZ
        |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
        |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
        |179 ORE => 7 PSHF
        |177 ORE => 5 HKGWZ
        |7 DCFZ, 7 PSHF => 2 XJWVT
        |165 ORE => 2 GPVTF
        |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
        |""".stripMargin.linesIterator.toList.tail
    val recipes = Day14.toRecipes(input)
    Day14.source(Set(Ingredient("FUEL", 1)), recipes) shouldEqual Set(Ingredient("ORE", 13312))
  }

  "Example 4" should "finish with correct result" in {
    val input =
      """
        |2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
        |17 NVRVD, 3 JNWZP => 8 VPVL
        |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
        |22 VJHF, 37 MNCFX => 5 FWMGM
        |139 ORE => 4 NVRVD
        |144 ORE => 7 JNWZP
        |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
        |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
        |145 ORE => 6 MNCFX
        |1 NVRVD => 8 CXFTF
        |1 VJHF, 6 MNCFX => 4 RFSQX
        |176 ORE => 6 VJHF
        |""".stripMargin.linesIterator.toList.tail
    val recipes = Day14.toRecipes(input)
    Day14.source(Set(Ingredient("FUEL", 1)), recipes) shouldEqual Set(Ingredient("ORE", 180697))
  }

  "Example 5" should "finish with correct result" in {
    val input =
      """
        |171 ORE => 8 CNZTR
        |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
        |114 ORE => 4 BHXH
        |14 VRPVC => 6 BMBT
        |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
        |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
        |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
        |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
        |5 BMBT => 4 WPTQ
        |189 ORE => 9 KTJDG
        |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
        |12 VRPVC, 27 CNZTR => 2 XDBXC
        |15 KTJDG, 12 BHXH => 5 XCVML
        |3 BHXH, 2 VRPVC => 7 MZWV
        |121 ORE => 7 VRPVC
        |7 XCVML => 6 RJRHP
        |5 BHXH, 4 VRPVC => 5 LTCX
        |""".stripMargin.linesIterator.toList.tail
    val recipes = Day14.toRecipes(input)
    Day14.source(Set(Ingredient("FUEL", 1)), recipes) shouldEqual Set(Ingredient("ORE", 2210736))
  }

  "Part 1" should "return the amount of ORE needed to produce 1 FUEL" in {
    Day14.result1.head.quantity shouldBe 483766
    Day14.oreForFuel(1) shouldBe 483766
  }

  "Part 2" should "return the greatest amount of FUEL needing < 1 trillion ORE" in {
    val trillion = 1000000000000L
    trillion / Day14.result1.head.quantity shouldBe 2067115

    Day14.oreForFuel(11759443)   shouldBe 999999992087L
    //                           trillion 1000000000000L
    Day14.oreForFuel(11759444)   shouldBe 1000000063895L

    val i = for {
      i <- LazyList.from(11758000, 1)
      if Day14.oreForFuel(i) > trillion
    } yield i - 1
    println(i.head)
    i.head shouldBe 11759443
  }
}

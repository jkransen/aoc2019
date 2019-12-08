package nl.kransen.aoc2019

import org.scalatest._

class Day8Spec extends FlatSpec with Matchers {

  import Day8._

  "Example from text" should "construct two layers" in {
    toImage("123456789012", 3, 2) shouldEqual
      (
        (
          (1 :: 2 :: 3 :: Nil) ::
          (4 :: 5 :: 6 :: Nil) ::
          Nil) ::
        (
          (7 :: 8 :: 9 :: Nil) ::
          (0 :: 1 :: 2 :: Nil) ::
          Nil) ::
        Nil)
  }

  "Part 1" should "finish with correct result" in {
    val image = source
    image.size shouldBe 100
    val fewest = fewestZeros(image)
    count(fewest, _ == 1) shouldBe 14
    count(fewest, _ == 2) shouldBe 130
    solution1 shouldBe 1820
  }

  "Part 2" should "example text" in {
    val raw = "0222112222120000"
    val image = toImage(raw, 2, 2)
    val flattened = flatten(image)
    flattened shouldEqual
      (
        (0 :: 1 :: Nil) ::
        (1 :: 0 :: Nil) ::
        Nil
      )
  }

  "Part 2 " should "solution " in {
    val flattened = flatten(source)
    val password = stringify(flattened)
    println(password)
    val expected =
      "#### #  # #  #  ##    ## \n" +
      "   # #  # # #  #  #    # \n" +
      "  #  #  # ##   #       # \n" +
      " #   #  # # #  #       # \n" +
      "#    #  # # #  #  # #  # \n" +
      "####  ##  #  #  ##   ##  "
    password shouldEqual expected
  }
}

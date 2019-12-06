package nl.kransen.aoc2019

import nl.kransen.aoc2019.Day6.CenterOfMass
import org.scalatest._

class Day6Spec extends FlatSpec with Matchers {

  "Example from text 1" should "have 12 masses" in {
    val exampleMap =
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L""".stripMargin
    val masses = Day6.toMasses(exampleMap.linesIterator.toList)
    masses.size shouldEqual 12
    masses.contains(CenterOfMass) shouldBe true
    masses.find(_.name.equals("J")).get.name.equals("E")
  }

  "Example from text 1" should "have 42 transitive orbits" in {
    val exampleMap =
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L""".stripMargin
    val orbits = Day6.transitiveOrbits(exampleMap.linesIterator.toList)
    orbits shouldEqual 42
  }

  "Solution 1" should "be" in {
    Day6.orbits shouldBe 135690
  }

  "Example from text 1" should "have 4 steps from YOU to SAN" in {

    val exampleMap =
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |K)YOU
        |I)SAN""".stripMargin
    Day6.steps("YOU", "SAN", exampleMap.linesIterator.toList) shouldEqual 4
  }

  "Solution 2" should "be 298" in {
    Day6.stepsToSatan shouldBe 298
  }
}

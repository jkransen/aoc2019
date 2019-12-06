package nl.kransen.aoc2019

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {

  sealed trait Mass {
    def name: String
    def orbits: Int
  }
  case class OrbitingMass(name: String, orbitee: Mass) extends Mass {
    override val orbits: Int = 1 + orbitee.orbits
  }
  case object CenterOfMass extends Mass {
    override def name: String = "COM"
    override val orbits = 0
  }

  @tailrec
  def addOrbiters(orbitees: Set[Mass], orbitMap: Map[String, String], collected: Set[Mass]): Set[Mass] = {
    if (orbitees.isEmpty) {
      collected
    } else {
      val nextMass = orbitees.head
      val orbitingMasses = orbitMap.filter(_._2.equals(nextMass.name)).map(orbiter => OrbitingMass(orbiter._1, nextMass)).toSet
      addOrbiters(orbitingMasses ++ orbitees.tail, orbitMap, collected ++ orbitingMasses)
    }
  }

  def toMasses(serialisation: List[String]): Set[Mass] = {
    val lineRegex = "(.+)\\)(.+)".r
    val orbitMap: Map[String, String] = serialisation
      .flatMap(line => lineRegex.findAllMatchIn(line))
      .map(m => m.group(2) -> m.group(1))
      .toMap
    addOrbiters(Set(CenterOfMass), orbitMap, Set(CenterOfMass))
  }

  def transitiveOrbits(serialisation: List[String]): Int = {
    toMasses(serialisation).foldLeft(0)((acc, next) => acc + next.orbits)
  }

  @tailrec
  def ancestors(child: Mass, foundAncestors: List[Mass] = List()): List[Mass]  = {
    child match {
      case CenterOfMass => child :: foundAncestors
      case orbiter @ OrbitingMass(_, orbitee) => ancestors(orbitee, orbiter :: foundAncestors)
    }
  }

  def steps(firstName: String, secondName: String, serialisation: List[String]): Int = {
    val masses = toMasses(serialisation)
    val first = masses.find(_.name.equals(firstName)).get
    val second = masses.find(_.name.equals(secondName)).get
    val ancestorsFirst = ancestors(first)
    val ancestorsSecond = ancestors(second)
    val ancestorsFirstSpecific = ancestorsFirst.filterNot(ancestorsSecond.contains(_))
    val ancestorsSecondSpecific = ancestorsSecond.filterNot(ancestorsFirst.contains(_))
    ancestorsFirstSpecific.size + ancestorsSecondSpecific.size - 2
  }

  def input: List[String] = {
    val source = Source.fromResource("day6/input.txt")
    val lines: List[String] = source.getLines().toList
    source.close()
    lines
  }

  lazy val orbits = transitiveOrbits(input)
  println(s"Transitive orbits: $orbits")

  lazy val stepsToSatan = steps("YOU", "SAN", input)
  println(s"Steps to Satan: $stepsToSatan")
}

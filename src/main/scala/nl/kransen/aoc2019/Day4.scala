package nl.kransen.aoc2019

import scala.annotation.tailrec

object Day4 extends App {

  @tailrec
  def hasDuplicate(digits: List[Int]): Boolean = {
    digits match {
      case first :: second :: _ => if (first == second) true else hasDuplicate(digits.tail)
      case _ => false
    }
  }

  @tailrec
  def hasExactDuplicate(digits: List[Int], duplicates: Set[Int] = Set(), thruplicates: Set[Int] = Set()): Boolean = {
    digits match {
      case first :: second :: _ if first == second =>
        hasExactDuplicate(digits.tail, duplicates = duplicates + first,
          thruplicates = if (duplicates.contains(first)) thruplicates + first else thruplicates)
      case _ :: _ => hasExactDuplicate(digits.tail, duplicates, thruplicates)
      case _ => (duplicates -- thruplicates).nonEmpty
    }
  }

  @tailrec
  def hasDecrease(digits: List[Int]): Boolean = {
    digits match {
      case first :: second :: _ => if (second < first) true else hasDecrease(digits.tail)
      case _ => false
    }
  }

  lazy val duplicates = for {
    i <- 197487 to 673251
    digits = i.toString.map(_.toInt).toList
    if hasDuplicate(digits)
    if !hasDecrease(digits)
  } yield i

  println(s"Number of duplicates: ${duplicates.size}")

  lazy val exactDuplicates = for {
    i <- 197487 to 673251
    digits = i.toString.map(_.toInt).toList
    if !hasDecrease(digits)
    if hasExactDuplicate(digits)
  } yield i

  println(s"Number of exact duplicates: ${exactDuplicates.size}")
}

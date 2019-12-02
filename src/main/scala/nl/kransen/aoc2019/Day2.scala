package nl.kransen.aoc2019

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  @tailrec
  def run(state: Array[Int], step: Int = 0): Array[Int] = {
    val pointer = 4 * step
    val op = state(pointer)
    if (op == 99) {
      state
    } else {
      val first = state(state(pointer + 1))
      val second = state(state(pointer + 2))
      val target = state(pointer + 3)
      if (op == 1) {
        state(target) = first + second
      } else if (op == 2) {
        state(target) = first * second
      } else {
        throw new Exception(s"Unknown op: $op")
      }
      run(state, step + 1)
    }
  }

  def initState(): Array[Int] = {
    val source = Source.fromResource("day2/input.txt")
    val state: Array[Int] = source.getLines()
      .toList.head.split(",").map(_.toInt)
    source.close()
    state
  }

  def initState(noun: Int, verb: Int): Array[Int] = {
    val state = initState()
    state.update(1, noun)
    state.update(2, verb)
    state
  }

  println(s"Output: ${run(initState()).mkString("[", ",", "]")}")

  lazy val resultsPart2 = for (
    noun <- 0 until 99;
    verb <- 0 until 99
    if run(initState(noun, verb), 0)(0) == 19690720
  ) yield noun * 100 + verb

  println(s"Result: ${resultsPart2.headOption}")
}

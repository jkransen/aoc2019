package nl.kransen.aoc2019

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {

  case class Ingredient(name: String, quantity: Long)

  case class Recipe(target: Ingredient, sources: Set[Ingredient])

  /**
   * Converts a target ingredient into the sources needed to get it.
   *
   * @param target The target ingredient and amount to be produced
   * @param recipes The recipes to go from ingredients to target.
   * @return The ingredients needed to produce the desired amount of target
   */
  def unprocess(target: Ingredient, recipes: Map[String, Recipe]): Set[Ingredient] = {
    val recipe = recipes(target.name)
    val times = math.ceil(target.quantity.toFloat / recipe.target.quantity).toInt
    recipe.sources.map(s => s.copy(quantity = s.quantity * times))
  }

  /**
   * Merges two sets of Ingredients, so that duplicates are replaced by the summed amounts.
   */
  @tailrec
  def merge(first: Set[Ingredient], second: Set[Ingredient]): Set[Ingredient] = {
    if (first.isEmpty) {
      second
    } else {
      val head = first.head
      second.find(i => i.name.equals(head.name)) match {
        case Some(s) => merge(first.tail, second - s + head.copy(quantity = head.quantity + s.quantity))
        case None => merge(first.tail, second + head)
      }
    }
  }

  /**
   * Recursively replaces the desired ingredients by the ingredients needed to get them, until only ORE remains.
   */
  @tailrec
  def source(output: Set[Ingredient], recipes: Map[String, Recipe]): Set[Ingredient] = {
    if (output.size == 1 && output.head.name == "ORE") {
      output
    } else {
      val remainingRecipeInputs = recipes.values.flatMap(_.sources).map(_.name).toSet
      val head = output.find(nxt => !remainingRecipeInputs.contains(nxt.name)).get
      val neededForHead = unprocess(head, recipes)
      source(merge(neededForHead, output - head), recipes - head.name)
    }
  }

  def toIngredient(str: String): Ingredient = {
    val chunks = str.trim.split(" ")
    Ingredient(chunks(1), chunks(0).toInt)
  }

  def toRecipes(input: List[String]): Map[String, Recipe] = {
    input.map { line =>
      line.split("=>").toList match {
        case sourcesCombined :: targetStr :: _ =>
          val sourcesStr = sourcesCombined.split(",").toSet
          val sources = sourcesStr.map(toIngredient)
          val target = toIngredient(targetStr)
          target.name -> Recipe(target, sources)
      }
    }.toMap
  }

  def source(): List[String] = {
    val source = Source.fromResource("day14/input.txt")
    val lines = source.getLines().toList
    source.close()
    lines
  }

  lazy val result1 = source(Set(Ingredient("FUEL", 1)), toRecipes(source()))
  println(result1)

  def oreForFuel(i: Long): Long = {
    val ore = source(Set(Ingredient("FUEL", i)), toRecipes(source()))
    ore.head.quantity
  }
}

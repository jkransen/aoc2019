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
   * Targets are only replaced if they are not needed as sources anymore. Their recipes are removed to
   * keep track of targets that were already unprocessed.
   */
  @tailrec
  def source(ingredients: Set[Ingredient], recipes: Map[String, Recipe]): Set[Ingredient] = {
    if (ingredients.size == 1 && ingredients.head.name == "ORE") {
      ingredients
    } else {
      val remainingRecipes = recipes.values.flatMap(_.sources).map(_.name).toSet
      val head = ingredients.find(next => !remainingRecipes.contains(next.name)).get
      val neededForHead = unprocess(head, recipes)
      source(merge(neededForHead, ingredients - head), recipes - head.name)
    }
  }

  /**
   * Turns a List of input lines into Recipes from sources to target.
   * @return A Map from target name to the Recipe to get to that target.
   */
  def toRecipes(input: List[String]): Map[String, Recipe] = {
    def toIngredient(str: String): Ingredient = {
      val chunks = str.trim.split(" ")
      Ingredient(chunks(1), chunks(0).toInt)
    }

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

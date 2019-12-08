package nl.kransen.aoc2019

import scala.io.Source

object Day8 extends App {

  type Row = List[Int]
  type Layer = List[Row]
  type Image = List[Layer]

  def count(layer: Layer, predicate: Int => Boolean): Int = {
    layer.map(_.count(predicate)).sum
  }

  def fewestZeros(image: Image): Layer =
    image.sortWith((layer1, layer2) => count(layer1, _ == 0) < count(layer2, _ == 0)).head

  def onesTimesTwos(image: Image): Int = {
    val selectedLayer: Layer = fewestZeros(image)
    count(selectedLayer, _ == 1) * count(selectedLayer, _ == 2)
  }

  def toImage(input: String, width: Int, height: Int): Image = {
    toImage(input.map(_.toInt - '0'), width, height)
  }

  def toImage(input: IndexedSeq[Int], width: Int, height: Int): Image = {
    val layerSize = width * height
    input.grouped(layerSize).map(toLayer(width, height)).toList
  }

  def toLayer(width: Int, height: Int)(input: IndexedSeq[Int]): Layer = {
    input.grouped(width).map(r => r.toList).toList
  }

  def source: Image = {
    val source = Source.fromResource("day8/input.txt")
    val image: Image = toImage(source.getLines().mkString, 25, 6)
    source.close()
    image
  }

  lazy val solution1 = onesTimesTwos(source)

  println(s"Ones times twos of fewest zeros: ${solution1}\n")

  def flattenPixels(pixel1: Int, pixel2: Int): Int = {
    if (pixel1 == 2) pixel2 else pixel1
  }

  def flattenRows(row1: Row, row2: Row): Row = {
    row1.zip(row2).map {
      case (one: Int, two: Int) => flattenPixels(one, two)
    }
  }

  def flattenLayers(layer1: Layer, layer2: Layer): Layer = {
    layer1.zip(layer2).map {
      case (one: Row, two: Row) => flattenRows(one, two)
    }
  }

  def flatten(image: Image): Layer = {
    image.reduce(flattenLayers)
  }

  def stringify(layer: Layer): String = {
    layer.map(row => row.map(i => if (i == 1) "#" else " ").mkString).mkString("\n")
  }

  println(stringify(flatten(source)))
}

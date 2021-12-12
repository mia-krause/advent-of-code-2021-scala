package adventofcode

import scala.io.Source

object Day5 {
  val day = 5
  val example = false

  def main(args: Array[String]): Unit = {
    val lines = readFile()

    val coordinates = lines.foldLeft(List[Coordinate]())((list, line) => list.appended(line.c1).appended(line.c2))
    val coordinatesBetween = lines.map(l => l.getIntermediateCoordinates()).flatten

    val diagram = createDiagram(coordinatesBetween)

    println(s"Overlaps: ${countOverlaps(diagram)}")

    }

  def readFile(): List[Line] = {
    var filename = s"src/main/resources/input_day${day}"
    if(example) filename += "_example"
    val source = Source.fromFile(filename)
    val list = source.getLines().map(Line(_)).toList
    source.close()
    list
  }

  def printTwoDimesionalArrToString[T](twoDimesionalArr: Array[Array[T]]) : Unit = {
    twoDimesionalArr.foreach(printOneDimesionalArrToString(_))
  }

  def printOneDimesionalArrToString[T](oneDimesionalArr: Array[T]) : Unit = {
    println(oneDimesionalArr.mkString(" "))
  }

  def createDiagram(coordinates:List[Coordinate]): Array[Array[Int]] = {
    val (maxX, maxY) = findMaxCoordinates(coordinates)
    coordinates.foldLeft(Array.fill(maxX+1, maxY+1)(0)){
      (arr, coordinate) => arr(coordinate.x)(coordinate.y) = arr(coordinate.x)(coordinate.y)+1;
      arr}
  }

  def findMaxCoordinates(coordinates:List[Coordinate]) = {
    val maxX = coordinates.map(c => c.x).max
    val maxY = coordinates.map(c => c.y).max
    (maxX, maxY)
  }

  def countOverlaps(diagram: Array[Array[Int]]) = {
    diagram.flatten.filter(i => i>1).length
  }
}

class Line(val c1: Coordinate, val c2: Coordinate) {

  def getIntermediateCoordinates() : List[Coordinate] = {
      val xRange = getRangeBetween(c1.x, c2.x)

      val yRange = getRangeBetween(c1.y, c2.y)

      if (xRange.length == yRange.length) {
        xRange.indices.map(i => new Coordinate(xRange(i), yRange(i))).toList
      } else if (yRange.isEmpty) {
        xRange.indices.map(i => new Coordinate(xRange(i), c1.y)).toList
      } else if (xRange.isEmpty) {
        yRange.indices.map(i => new Coordinate(c1.x, yRange(i))).toList
      } else {
        println(s"WTF? c1: $c1, c2: $c2")
        List()
      }
  }

  def getRangeBetween(a: Int, b:Int) : List[Int] =
    if(a==b) List()
    else if(a<b) (a to b).toList
    else (b to a).reverse.toList


  override def toString: String = s"$c1 -> $c2"

}

object Line {
  def apply(s: String): Line = {
    val coordinates = s.split(" -> ").map(input => Coordinate(input))
    new Line(coordinates.head, coordinates.last)
  }
}

class Coordinate (val x: Int, val y: Int) {
  override def toString: String = s"($x, $y)"
}

object Coordinate {
  def apply(s: String): Coordinate = {
    val input = s.split(",").map(_.toInt)
    new Coordinate(input.head, input.last)
  }
}

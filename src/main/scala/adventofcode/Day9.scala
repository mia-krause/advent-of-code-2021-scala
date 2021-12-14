package adventofcode

import scala.io.Source

object Day9 {
  val day = 9
  val example = false
  var i = 0

  def main(args: Array[String]): Unit = {
    val topology = readFile()
    val lowPoints = findLowPoints(topology)
    println(lowPoints)

    //Part 1
//    val sumOfLowPoints = lowPoints.map(coord => topology(coord.y)(coord.x)+1).sum
//    println(s"Low Points: ${sumOfLowPoints}")

    //Part 2
    val basinLengths = findBasins(topology, lowPoints).map(_.size).sorted
    println(basinLengths)
    val productOfBiggestBasins = basinLengths.takeRight(3).product
    println(s"Product of biggest basins: ${productOfBiggestBasins}")
  }

  def readFile(): Array[Array[Int]] = {
    var filename = s"src/main/resources/input_day${day}"
    if (example) filename += "_example"
    val source = Source.fromFile(filename)
    val list = source.getLines().map(line => line.toCharArray.map(_.asDigit).toArray).toArray
    source.close()
    list
  }

  def findLowPoints(arr: Array[Array[Int]]): List[Coordinate] = {
    var lowPoints = List[Coordinate]()
    for (y <- 0 until arr.length) {
      for (x <- 0 until arr(y).length) {
        val currentValue = arr(y)(x)
        val surroundingCoordinates = SurroundingCoordinates(new Coordinate(x, y), arr)
        val allPossibleValuesAreHigher = surroundingCoordinates.surroundingCoordToValue.filter((_, value) => value.isDefined).forall((_, value) => value.get > currentValue)
        if (allPossibleValuesAreHigher)
          lowPoints = lowPoints.appended(new Coordinate(x, y))
      }
    }
    lowPoints
  }

  class Coordinate(val x: Int, val y: Int) {
    override def toString: String = s"($x, $y)"

    override def equals(obj: Any): Boolean = obj match {
      case other:Coordinate => x == other.x && y == other.y
      case _ => false
    }

    override def hashCode(): Int = (x.toChar.toInt*'~'.toInt)+y.toChar.toInt
  }

  class SurroundingCoordinates(val coord:Coordinate, val surroundingCoordToValue: Map[Coordinate, Option[Int]])

  object SurroundingCoordinates {
    def apply(coordinate: Coordinate, topology: Array[Array[Int]]) = {
      val x = coordinate.x
      val y = coordinate.y
      val currentValue = topology(y)(x)
      var surroundingCoordToValue = Map[Coordinate, Option[Int]]()

      val topValue = if (y > 0) Some(topology(y - 1)(x)) else None
      surroundingCoordToValue = surroundingCoordToValue + (new Coordinate(x, y-1) -> topValue)

      val bottomValue = if (y < topology.length - 1) Some(topology(y + 1)(x)) else None
      surroundingCoordToValue = surroundingCoordToValue + (new Coordinate(x, y+1) -> bottomValue)

      val leftValue = if (x > 0) Some(topology(y)(x - 1)) else None
      surroundingCoordToValue = surroundingCoordToValue + (new Coordinate(x-1, y) -> leftValue)

      val rightValue = if (x < topology(y).length - 1) Some(topology(y)(x + 1)) else None
      surroundingCoordToValue = surroundingCoordToValue + (new Coordinate(x+1, y) -> rightValue)

       new SurroundingCoordinates(new Coordinate(x, y), surroundingCoordToValue)
    }

  }

  def findBasins(arr: Array[Array[Int]], lowPoints: List[Coordinate]): List[List[Coordinate]] = {
    var basins =  List[List[Coordinate]]()
    for(lowPoint <- lowPoints) { //x, y
      basins = basins.appended(findBasin(arr, lowPoint))
    }
    basins
  }

  def findBasin(arr: Array[Array[Int]], lowPoint: Coordinate): List[Coordinate] = {
    println(s"lowPoint: $lowPoint -> value: ${arr(lowPoint.y)(lowPoint.x)}")
    var coordsInBasin = List(lowPoint)

    val surroundingCoordinates = SurroundingCoordinates(lowPoint, arr)
    val currentValue = arr(surroundingCoordinates.coord.y)(surroundingCoordinates.coord.x)
    val nextLowPoints: List[Coordinate] = surroundingCoordinates.surroundingCoordToValue
      .filter((_, value) => value.isDefined && value.get < 9 && value.get == currentValue+1)
      .keys.toList

    nextLowPoints.foreach(coord => coordsInBasin = coordsInBasin ++ findBasin(arr, coord))
    coordsInBasin.distinct
  }

  def printTwoDimesionalArrToString[T](twoDimesionalArr: Array[Array[T]]): Unit = {
    twoDimesionalArr.foreach(printOneDimesionalArrToString(_))
  }

  def printOneDimesionalArrToString[T](oneDimesionalArr: Array[T]): Unit = {
    println(oneDimesionalArr.mkString(""))
  }
}

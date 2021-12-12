package adventofcode

import scala.io.Source

object Day7 {
  val day = 7
  val example = false

  def main(args: Array[String]): Unit = {
    val crabPositions = readFile()
    val maxPosition = crabPositions.max
    println((0 until maxPosition).map(pos => calcFuelCosts2(crabPositions, pos)).min)
    }

  def readFile(): List[Int] = {
    var filename = s"src/main/resources/input_day${day}"
    if(example) filename += "_example"
    val source = Source.fromFile(filename)
    val list = source.getLines().toList.head.trim.split(",").map(_.toInt).toList
    source.close()
    list
  }

  def printTwoDimesionalArrToString[T](twoDimesionalArr: Array[Array[T]]) : Unit = {
    twoDimesionalArr.foreach(printOneDimesionalArrToString(_))
  }

  def printOneDimesionalArrToString[T](oneDimesionalArr: Array[T]) : Unit = {
    println(oneDimesionalArr.mkString(" "))
  }

  def calcFuelCosts1(crabPositions: List[Int], targetPosition: Int) = crabPositions.map(crabPos => Math.abs(crabPos-targetPosition)).sum

  def calcFuelCosts2(crabPositions: List[Int], targetPosition: Int) = crabPositions.map(crabPos => calcFuelCost2(Math.abs(crabPos-targetPosition))).sum

  def calcFuelCost2(distance: Int): Int = {
    if (distance <= 1) return distance
    distance + calcFuelCost2(distance - 1)
  }
}

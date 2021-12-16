package adventofcode

import scala.io.Source

object Day11 {
  val day = 11
  val example = false

  def main(args: Array[String]): Unit = {
    var octopi = readFile()
    var flashed: List[(Int, Int)] = List()
    printTwoDimesionalArrToString(octopi)
    var success = false
    var stepNumber = 0
    while(!success) {
      stepNumber = stepNumber + 1
      val (currentOctopi, _) = step(octopi)
      octopi = currentOctopi
      success=currentOctopi.flatten.sum==0
    }
    println(s"\n${stepNumber}")
  }

  def step(octopi: Array[Array[Int]]): (Array[Array[Int]], List[(Int, Int)]) = {
    octopi.mapInPlace(_.mapInPlace(energy => energy + 1))
    val (flashedOctopi: Array[Array[Int]], alreadyFlashed) = flash(octopi, List())
    flashedOctopi.mapInPlace(_.mapInPlace(energy => if (energy > 9) 0 else energy))

    (flashedOctopi, alreadyFlashed)
  }

  def flash(octopi: Array[Array[Int]], alreadyFlashed: List[(Int, Int)]): (Array[Array[Int]], List[(Int, Int)]) = {
    val containsUnflashed = octopi.indices.exists(i => octopi(i).indices.exists(j => octopi(i)(j)> 9 && !alreadyFlashed.contains((i, j))))
    if(!containsUnflashed) return (octopi, alreadyFlashed)

    var flashed = alreadyFlashed

    for (i <- octopi.indices) {
    for (j <- octopi(i).indices) {
      if (octopi(i)(j) > 9 && !flashed.contains((i, j))) {
        for (surroundingI <- (i - 1 to i + 1)) {
          if ((0 until octopi.length).contains(surroundingI)) {
            for (surroundingJ <- (j - 1 to j + 1)) {
              if ((0 until octopi(surroundingI).length).contains(surroundingJ)) {
                octopi(surroundingI)(surroundingJ) = octopi(surroundingI)(surroundingJ) + 1
              }
            }
          }
        }
        flashed = flashed.appended(i, j)
      }
    }
  }
    flash(octopi, flashed)
  }

  def readFile(): Array[Array[Int]] = {
    var filename = s"src/main/resources/input_day${day}"
    if (example) filename += "_example"
    val source = Source.fromFile(filename)
    val octopi = source.getLines().map(line => line.toCharArray.map(_.asDigit).toArray).toArray
    source.close()
    octopi
  }

  def printTwoDimesionalArrToString(twoDimesionalArr: Array[Array[Int]]): Unit = {
    twoDimesionalArr.foreach(printOneDimesionalArrToString(_))
  }

  def printOneDimesionalArrToString(oneDimesionalArr: Array[Int]): Unit = {
    println(oneDimesionalArr.map(i => f"${i}%2d").mkString(" "))
  }
}

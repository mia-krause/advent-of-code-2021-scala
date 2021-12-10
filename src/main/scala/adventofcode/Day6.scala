package adventofcode

import scala.io.Source

object Day6 {
  val day = 6
  val example = false

  def main(args: Array[String]): Unit = {
    val fishes = readFile()
    val part1 = (0 until 80).foldLeft(fishes){(list, _) => nextDay(list)}
    val part2 = (0 until 256).foldLeft(fishes){(list, _) => nextDay(list)}

    println(part1.sum)
    println(part2.sum)
  }

  def readFile(): List[Long] = {
    var filename = s"src/main/resources/input_day${day}"
    if(example) filename += "_example"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()
    lines.head.trim.split(",").map(_.toInt).foldLeft(Array.fill(9)(0)){(arr: Array[Int], i: Int) => arr(i) = arr(i) +1; arr}.map(_.toLong).toList
  }

  def nextDay(fishes: List[Long]): List[Long] = {
    val reproducingFishes = fishes.head
    var nextFishes = fishes.drop(1)
    nextFishes = nextFishes.updated(6, nextFishes(6)+reproducingFishes)
    nextFishes = nextFishes.appended(reproducingFishes)
    nextFishes
  }
}

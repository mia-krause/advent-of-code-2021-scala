package adventofcode

import scala.io.Source
import scala.language.postfixOps

object Day4 {
  val day = 4

  def main(args: Array[String]): Unit = {
    val fileInput = readFile()
    val (numbers, bingoFields) = parseFile(fileInput)

    var winners = List[Int]()

    bingoFields.indices.foreach(j =>
      println("\n" + bingoFields(j).map(arr => arr.mkString("", " ", "")).mkString("", "\n ", "")))

    var markedFields = initMarkedFields(bingoFields.size)

    var lastSuccessfulNumber = Int.MaxValue

    var i = 0
    while (i < numbers.length) {
      markedFields = bingoFields.indices.map(j => {
        if(winners.contains(j)) markedFields(j)
        else markField(bingoFields(j), markedFields(j), numbers(i))
      }).toList
      markedFields.indices.foreach(j =>
        if (evaluate(markedFields(j)) && winners.size < bingoFields.size && !winners.contains(j)) {
          println(j)
          winners = winners.appended(j)
          lastSuccessfulNumber = numbers(i)
        })
      i += 1
    }

    val lastWinner = winners.last
    val sum = sumOfUnmarkedFields(bingoFields(lastWinner), markedFields(lastWinner))

    println("\n" +lastWinner)
    println(sum)
    println(lastSuccessfulNumber)
    println(sum * lastSuccessfulNumber)
  }

  def readFile(): List[String] = {
    val source = Source.fromFile(s"src/main/resources/input_day${day}")
    val list = source.getLines().toList
    source.close()
    list
  }

  def parseFile(list: List[String]): (Array[Int], List[Array[Array[Int]]]) = {
    val inputNumbers = list.head.split(",").map(_.toInt)

    val bingoFields = list.drop(2).sliding(5, 6)
      .map(x => x.map(s => s.trim.split("\\s+").map(_.toInt)).toArray)
      .toList

    (inputNumbers, bingoFields)
  }

  def markField(field: Array[Array[Int]], marked: Array[Array[Boolean]], number: Int): Array[Array[Boolean]] = {
    val newMarked = marked
    for (i <- 0 until 5) {
      for (j <- 0 until 5) {
        if (field(i)(j) == number) newMarked(i)(j) = true
      }
    }
    newMarked
  }

  def initMarkedFields(n: Int): List[Array[Array[Boolean]]] = {
    (0 until n).map(_ => Array.fill(5) {
      Array.fill(5) {
        false
      }
    }).toList
  }

  def evaluate(markedField: Array[Array[Boolean]]): Boolean = {
    for i <- 0 until 5 do
      if (0 until 5).map(markedField(i)(_)).reduce(_ && _) then
        return true
      if (0 until 5).map(markedField(_)(i)).reduce(_ && _) then
        return true
    false
  }

  def sumOfUnmarkedFields(square: Array[Array[Int]], markedFields: Array[Array[Boolean]]): Int = {
    var sum = 0
    for (i <- 0 until 5) {
      for (j <- 0 until 5) {
        if (!markedFields(i)(j)) sum += square(i)(j)
      }
    }
    sum
  }
}

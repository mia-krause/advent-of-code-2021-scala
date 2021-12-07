package adventofcode

import scala.io.Source
import scala.runtime.ScalaRunTime
import scala.util.control.Breaks.break

object App {
  def main(args: Array[String]): Unit = {
    val listOfArrays = readFile().toStream.map(s => s.split("").map(_.toInt)).toList
    var reducedGammaList = listOfArrays
    var bits: Array[Int] = getMostCommonBits(reducedGammaList)
    for(i <- listOfArrays.head.indices) {
      if(reducedGammaList.length>1) {
        reducedGammaList = reducedGammaList.filter(_(i) == bits(i))
        bits = getMostCommonBits(reducedGammaList)
      }
    }
    val gamma = Integer.parseInt(reducedGammaList.head.mkString, 2)

    var reducedEpsilonList = listOfArrays
    bits = getMostCommonBits(reducedEpsilonList)
    for(i <- listOfArrays.head.indices) {
      if(reducedEpsilonList.length>1) {
        reducedEpsilonList = reducedEpsilonList.filter(_ (i) != bits(i))
        bits = getMostCommonBits(reducedEpsilonList)
      }
    }
    val epsilon = Integer.parseInt(reducedEpsilonList.head.mkString, 2)

    println(gamma*epsilon)
  }


  private def getMostCommonBits(list: List[Array[Int]]) = {
    var sum = Array.fill[Int](list.head.length)(0)
    list.foreach(arr => for i <- list.head.indices do sum(i) = sum(i) + arr(i))
    val half: Double =  list.length/ 2.0
    sum.map(j => if (j >= half) 1 else 0)
  }

  def readFile(): List[String] = {
    val source = Source.fromFile("src/main/resources/input_day3")
    val list = source.getLines().toList
    source.close()
    list
  }
}

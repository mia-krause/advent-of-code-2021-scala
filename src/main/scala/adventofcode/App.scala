package adventofcode

import scala.io.Source
import scala.runtime.ScalaRunTime

object App {
  def main(args: Array[String]): Unit = {
    println("Hello World")
    val listOfArrays = readFile().toStream.map(s => s.split("").map(_.toInt)).toList
    val sum = Array.fill[Int](12)(0)
    listOfArrays.foreach(arr => for i <- 0 to 11 do sum(i) = sum(i)+arr(i))
    val bits = sum.map(i => if(i < listOfArrays.length/2) 0 else 1)
    val bitsInverse = sum.map(i => if(i < listOfArrays.length/2) 1 else 0)
    val gamma = Integer.parseInt(bits.mkString, 2)
    val epsilon = Integer.parseInt(bitsInverse.mkString, 2)
    println(sum.mkString("Array(", ", ", ")"))
    println(bits.mkString("Array(", ", ", ")"))
    println(bitsInverse.mkString("Array(", ", ", ")"))
    println(gamma)
    println(epsilon)
    println(epsilon*gamma)
  }



  def readFile(): List[String] = {
    val source = Source.fromFile("src/main/resources/input_day3")
    val list = source.getLines().toList
    source.close()
    list
  }
}

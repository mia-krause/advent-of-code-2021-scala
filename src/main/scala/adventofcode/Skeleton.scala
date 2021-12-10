package adventofcode

import scala.io.Source

object Skeleton {
  val day = 0
  val example = true

  def main(args: Array[String]): Unit = {
    val fileInput = readFile()
    }

  def readFile(): List[String] = {
    var filename = s"src/main/resources/input_day${day}"
    if(example) filename += "_example"
    val source = Source.fromFile(filename)
    val list = source.getLines().toList
    source.close()
    list
  }
}

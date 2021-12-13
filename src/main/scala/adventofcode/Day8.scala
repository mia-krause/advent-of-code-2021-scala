package adventofcode

import scala.io.Source

object Day8 {
  val day = 8
  val example = false

  def main(args: Array[String]): Unit = {
    val entries: List[Entry] = readFile()

    val mappedEntries = entries.map(entry => interpretValues(findMapping(entry.signalPattern), entry.outputValues)).map(concatListToInt(_)).toList
    println(mappedEntries.sum)
    }

  def readFile(): List[Entry] = {
    var filename = s"src/main/resources/input_day${day}"
    if(example) filename += "_example"
    val source = Source.fromFile(filename)
    val list = source.getLines().map(Entry.from(_)).toList
    source.close()
    list
  }

  def interpretValues(mapping:Map[String, Int], digitsToInterpret: List[String]): List[Int] = {
    digitsToInterpret.map(digit => mapping.get(digit)).flatten.toList
  }

  def findMapping(digitsToMap: List[String]): Map[String, Int] = {
    val (simpleMappings, complexDigits) = findSimpleMappings(digitsToMap)
    findComplexMappings(simpleMappings, complexDigits).map((k:Int, v:String) => v -> k).toMap
  }

  def findComplexMappings(currentMapping: Map[Int, String], digitsToMap: List[String]): Map[Int, String] = {
    if(digitsToMap.isEmpty) return currentMapping
    val digit = digitsToMap.head
    val subSetsOfDigit: Map[Int, String] = currentMapping.filter((number:Int, mappedDigit: String) => containsChars(digit, mappedDigit))
    if(digit.length==5) {
      if(subSetsOfDigit.get(1).isDefined) return findComplexMappings(currentMapping+(3 -> digit), digitsToMap.drop(1))
      val difference1And4 = removeChars(currentMapping.get(4).get, currentMapping.get(1).get)
      if(containsChars(digit, difference1And4)) return findComplexMappings(currentMapping+(5 -> digit), digitsToMap.drop(1))
      return findComplexMappings(currentMapping+(2 -> digit), digitsToMap.drop(1))
    }
    if(digit.length==6) {
      if(subSetsOfDigit.get(4).isDefined) return findComplexMappings(currentMapping+(9 -> digit), digitsToMap.drop(1))
      if(subSetsOfDigit.get(1).isDefined) return findComplexMappings(currentMapping+(0 -> digit), digitsToMap.drop(1))
      return findComplexMappings(currentMapping+(6 -> digit), digitsToMap.drop(1))
    }
    println("WTF")
    ???
  }

  def findSimpleMappings(digitsToMap: List[String]): (Map[Int, String], List[String]) = {
    val digitsWithSimpleMapping: Map[String, Option[Int]] = digitsToMap.map(digit => digit.length match {
      case 2 => digit -> Some(1)
      case 3 => digit -> Some(7)
      case 4 => digit -> Some(4)
      case 7 => digit -> Some(8)
      case _ =>  digit -> None
    }).toMap
    val complexDigits: List[String] = digitsWithSimpleMapping.view.filter((_, number) => number.isEmpty).map((digit, _) => digit).toList
    val simpleMappings: Map[Int, String] = digitsWithSimpleMapping.view.filter((_, number) => number.isDefined)
      .map((digit, number) => number.get -> digit).toMap
    (simpleMappings, complexDigits)
  }

  def containsChars(string: String, charsToLookUp: String): Boolean = {
    charsToLookUp.toCharArray.forall(c => string.contains(c))
  }

  def removeChars(string: String, charsToRemove: String): String = {
    var newString = string
    charsToRemove.toCharArray.foreach(c => newString = newString.replace(c, ' '))
    newString.replaceAll("\\s", "")
  }

  def concatListToInt(list: List[Int]) = list.mkString("").toInt

  class Entry(val signalPattern: List[String], val outputValues: List[String]) {
    override def toString() = "Signal Pattern: " + signalPattern.mkString("(", ", ", ")") + ", Output Value: " + outputValues

  }

  object Entry{
    def from(s: String) = {
      val signalPattern: List[String] = s.split("\\|").head.trim.split(" ").map(_.toCharArray.sorted.mkString).toList
      val outputValues: List[String] = s.split("\\|").last.trim.split(" ").map(_.toCharArray.sorted.mkString).toList
      new Entry(signalPattern, outputValues)
    }
  }
}

package adventofcode

import scala.io.Source

object Day10 {
  val day = 10
  val example = true

  enum BracketType {
    case ROUND, SQUARE, CURLY, ANGLE
  }

  enum BracketFunction {
    case OPENING, CLOSING
  }

  class Bracket(val typ: BracketType, val function: BracketFunction) {
    override def toString: String = Bracket.mapping.filter((_, bracket) => this.equals(bracket)).keys.head.toString
  }

  object Bracket {
    val mapping = Map(
      '(' -> new Bracket(BracketType.ROUND, BracketFunction.OPENING),
      ')' -> new Bracket(BracketType.ROUND, BracketFunction.CLOSING),
      '[' -> new Bracket(BracketType.SQUARE, BracketFunction.OPENING),
      ']' -> new Bracket(BracketType.SQUARE, BracketFunction.CLOSING),
      '{' -> new Bracket(BracketType.CURLY, BracketFunction.OPENING),
      '}' -> new Bracket(BracketType.CURLY, BracketFunction.CLOSING),
      '<' -> new Bracket(BracketType.ANGLE, BracketFunction.OPENING),
      '>' -> new Bracket(BracketType.ANGLE, BracketFunction.CLOSING)
    )

    def apply(c: Char) = mapping.get(c).get

  }

  def main(args: Array[String]): Unit = {
    val input = readFile()
  }

  def readFile(): List[Bracket] = {
    var filename = s"src/main/resources/input_day${day}"
    if (example) filename += "_example"
    val source = Source.fromFile(filename)
    val input = source.getLines.mkString
    source.close()
    input.toCharArray.map(Bracket(_)).toList
  }
}

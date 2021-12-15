package adventofcode

import scala.collection.mutable
import scala.io.Source

object Day10 {
  val day = 10
  val example = false

  enum BracketType(val weight: Int) {
    case ROUND extends BracketType(1)
    case SQUARE extends BracketType(2)
    case CURLY extends BracketType(3)
    case ANGLE extends BracketType(4)
  }

  enum BracketFunction {
    case OPENING, CLOSING
  }

  def main(args: Array[String]): Unit = {
    val input = readFile()
    val completionStrings = input.map(findCompletionString(_)).filter(!_.isEmpty)
    completionStrings.foreach(println(_))
    val evaluatedCompletionStrings = completionStrings.map(evaluateCompletionString(_)).sorted

    println(evaluatedCompletionStrings((evaluatedCompletionStrings.length/2).toInt))
  }

  class Bracket(val typ: BracketType, val function: BracketFunction) {
    override def toString: String = {
      val filteredMapping = Bracket.mapping.filter((_, bracket: Bracket) => this.equals(bracket)).keys
        filteredMapping.head.toString
    }

    override def equals(obj: Any): Boolean = obj match {
      case Bracket(t,f) => typ==t && function==f
      case _ => false
    }

    override def hashCode(): Int = typ.weight*function.ordinal
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

    def unapply(bracket: Bracket) = (bracket.typ, bracket.function)

  }

  def readFile(): List[List[Bracket]] = {
    var filename = s"src/main/resources/input_day${day}"
    if (example) filename += "_example"
    val source = Source.fromFile(filename)
    val input = source.getLines.toList
    source.close()
    input.map(_.toCharArray.map(Bracket(_)).toList).toList
  }

  def findCorruptedBrackets(brackets: List[Bracket]): List[Bracket] = {
    val unclosedBrackets = mutable.Stack[BracketType]()
    var corruptedBrackets = List[Bracket]()
    for(bracket <- brackets) {
      bracket match {
        case Bracket(_, BracketFunction.OPENING) => unclosedBrackets.push(bracket.typ)
        case Bracket(t, BracketFunction.CLOSING) => {
          val lastUnclosedBracket = unclosedBrackets.pop()
          if(lastUnclosedBracket!=t) corruptedBrackets = corruptedBrackets.appended(bracket)
        }
      }
    }
    corruptedBrackets
  }

  def findCompletionString(brackets: List[Bracket]) : List[BracketType] = {
    val unclosedBrackets = mutable.Stack[BracketType]()
    for(bracket <- brackets) {
      bracket match {
        case Bracket(_, BracketFunction.OPENING) => unclosedBrackets.push(bracket.typ)
        case Bracket(t, BracketFunction.CLOSING) => {
          val lastUnclosedBracket = unclosedBrackets.pop()
          if(lastUnclosedBracket!=t) return List()
        }
      }
    }
    unclosedBrackets.toList
  }

  def evaluateCompletionString(brackets: List[BracketType]): Long = {
    var weights = brackets.map(_.weight)
    weights.foldLeft(0l)((total, nextWeight) => (total*5)+nextWeight)
  }
}

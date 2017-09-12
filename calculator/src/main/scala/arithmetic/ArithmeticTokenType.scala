package arithmetic

import interface.TokenType

import scala.util.matching.Regex

sealed abstract class ArithmeticTokenType extends TokenType {
  ArithmeticTokenType.instances :+= this
}

object ArithmeticTokenType {
  private var instances: List[ArithmeticTokenType] = List.empty
  def getInstances: List[ArithmeticTokenType] = instances

  // A weird initialization. Should I use enum instead?
  OpeningBracket
  ClosingBracket
  Plus
  Minus
  Multiplication
  Division
  Number
}

case object OpeningBracket extends ArithmeticTokenType {
  override val regex: Regex = raw"\(".r
}

case object ClosingBracket extends ArithmeticTokenType {
  override val regex: Regex = raw"\)".r
}

case object Plus extends ArithmeticTokenType {
  override val regex: Regex = raw"\+".r
}

case object Minus extends ArithmeticTokenType {
  override val regex: Regex = "-".r
}

case object Multiplication extends ArithmeticTokenType {
  override val regex: Regex = raw"\*".r
}

case object Division extends ArithmeticTokenType {
  override val regex: Regex = "/".r
}

case object Number extends ArithmeticTokenType {
  override val regex: Regex = raw"(\d*\.)?(\d)+".r
}
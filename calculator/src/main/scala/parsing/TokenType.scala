package parsing

import scala.util.matching.Regex

sealed trait TokenType {
  val regex: Regex
  TokenType.instances :+= this
}

object TokenType {
  private var instances: List[TokenType] = List.empty
  def getInstances: List[TokenType] = instances

  // A weird initialization. Should I use enum instead?
  OpeningBracket
  ClosingBracket
  Plus
  Minus
  Mul
  Div
  Number
}

case object OpeningBracket extends TokenType {
  override val regex: Regex = raw"\(".r
}

case object ClosingBracket extends TokenType {
  override val regex: Regex = raw"\)".r
}

case object Plus extends TokenType {
  override val regex: Regex = raw"\+".r
}

case object Minus extends TokenType {
  override val regex: Regex = "-".r
}

case object Mul extends TokenType {
  override val regex: Regex = raw"\*".r
}

case object Div extends TokenType {
  override val regex: Regex = "/".r
}

case object Number extends TokenType {
  override val regex: Regex = raw"(\d*\.)?(\d)+".r
}
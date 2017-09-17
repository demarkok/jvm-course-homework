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

sealed trait Operator extends TokenType {
  val priority: Int
  val isLeftAssoc: Boolean
}

case object OpeningBracket extends TokenType {
  override val regex: Regex = raw"\(".r
}

case object ClosingBracket extends TokenType {
  override val regex: Regex = raw"\)".r
}

case object Plus extends Operator {
  override val regex: Regex = raw"\+".r
  override val priority = 1
  override val isLeftAssoc = true
}

case object Minus extends Operator {
  override val regex: Regex = "-".r
  override val priority = 1
  override val isLeftAssoc = true
}

case object Mul extends Operator {
  override val regex: Regex = raw"\*".r
  override val priority = 2
  override val isLeftAssoc = true
}

case object Div extends Operator {
  override val regex: Regex = "/".r
  override val priority = 2
  override val isLeftAssoc = true
}

case object Number extends TokenType {
  override val regex: Regex = raw"(\d*\.)?(\d)+".r
}
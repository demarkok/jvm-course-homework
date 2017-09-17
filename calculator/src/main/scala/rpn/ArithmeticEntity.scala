package rpn

import parsing.ArithmeticToken

sealed trait ArithmeticEntity

object ArithmeticEntity {
  def apply(token: ArithmeticToken): ArithmeticEntity = {
    token match {
      case ArithmeticToken(parsing.OpeningBracket, _) => OpeningBracket
      case ArithmeticToken(parsing.ClosingBracket, _) => ClosingBracket
      case ArithmeticToken(parsing.Plus, _) => Plus
      case ArithmeticToken(parsing.Minus, _) => Minus
      case ArithmeticToken(parsing.Mul, _) => Mul
      case ArithmeticToken(parsing.Div, _) => Div
      case ArithmeticToken(parsing.Number, value) => Number(value.toDouble)
    }
  }
}

case object OpeningBracket extends ArithmeticEntity

case object ClosingBracket extends ArithmeticEntity

trait Operator extends ArithmeticEntity {
  val priority: Int
  val isLeftAssoc: Boolean
}

case object Plus extends Operator {
  override val priority = 1
  override val isLeftAssoc = true
}

case object Minus extends Operator {
  override val priority = 1
  override val isLeftAssoc = true
}

case object Mul extends Operator {
  override val priority = 2
  override val isLeftAssoc = true
}

case object Div extends Operator {
  override val priority = 2
  override val isLeftAssoc = true
}

case class Number(value: Double) extends ArithmeticEntity

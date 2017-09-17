package rpn

import parsing.Token

sealed trait ArithmeticEntity

object ArithmeticEntity {
  def apply(token: Token): ArithmeticEntity = {
    token match {
      case Token(parsing.OpeningBracket, _) => OpeningBracket
      case Token(parsing.ClosingBracket, _) => ClosingBracket
      case Token(parsing.Plus, _) => Plus
      case Token(parsing.Minus, _) => Minus
      case Token(parsing.Mul, _) => Mul
      case Token(parsing.Div, _) => Div
      case Token(parsing.Number, value) => Number(value.toDouble)
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

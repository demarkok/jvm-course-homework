package parsing

case class ArithmeticToken(tokenType: ArithmeticTokenType, value: String) extends Token(tokenType, value)

object ArithmeticToken {
  def apply(maybeTokenType: Option[_ <: ArithmeticTokenType], maybeValue: Option[String]): Option[ArithmeticToken] = {
    for {
      tokenType <- maybeTokenType
      value <- maybeValue
    } yield ArithmeticToken(tokenType, value)
  }
}

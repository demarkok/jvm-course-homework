package interface


class Token(val tokenType: TokenType, val value: String)

object Token {
  def apply(maybeTokenType: Option[_ <: TokenType], maybeValue: Option[String]): Option[Token] = {
    for {
      tokenType <- maybeTokenType
      value <- maybeValue
    } yield new Token(tokenType, value)
  }
}

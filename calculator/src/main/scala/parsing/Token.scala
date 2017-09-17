package parsing

class Token(tokenType: TokenType, value: String)

object Token {
  def apply(maybeTokenType: Option[_ <: TokenType], maybeValue: Option[String]): Option[Token] = {
    for {
      tokenType <- maybeTokenType
      value <- maybeValue
    } yield new Token(tokenType, value)
  }
}

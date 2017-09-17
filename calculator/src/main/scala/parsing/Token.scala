package parsing

/**
  * The class representing arithmetic token (short piece of input which makes sense in the context)
  * @param tokenType Type of the token (e.g. operation, bracket etc).
  * @param value Piece of string itself.
  */
case class Token(tokenType: TokenType, value: String)

/**
  * The companion object for Token class.
  */
object Token {
  /**
    * Construct optional Token from optional arguments. None value means unrecognized token.
    * If at least one argument is None, then the result is also None. Otherwise result is Some Token.
    *
    * @param maybeTokenType Optional token type.
    * @param maybeValue Optional token value.
    * @return Wrapped Token if both of the arguments are presented. Otherwise returns None.
    */
  def apply(maybeTokenType: Option[_ <: TokenType], maybeValue: Option[String]): Option[Token] = {
    for { // the evaluation in Option monad.
      tokenType <- maybeTokenType
      value <- maybeValue
    } yield Token(tokenType, value)
  }
}

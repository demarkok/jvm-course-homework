package parsing

import scala.util.matching.Regex

object Tokenizer {

  lazy val regex: Regex = {
    TokenType.getInstances
      .map(tokenType => "(?<" + tokenType.toString + ">" + tokenType.regex.regex + ")")
      .mkString("|")
      .r
  }

 def tokenize(rawString: String): Option[List[Token]] = {
    val string = rawString.trim
    if (string.isEmpty) {
      Some(List.empty)
    } else {
      val maybeMatchResult = regex.findPrefixMatchOf(string)
      val maybeMatchedString = maybeMatchResult.map(_.matched)
      val maybeTokenType = TokenType.getInstances
        .find(tokenType => maybeMatchResult.exists(_.group(tokenType.toString) != null))
      val maybeToken = Token(maybeTokenType, maybeMatchedString)
      for {
        token <- maybeToken
        matchedString <- maybeMatchedString
        restString = string.substring(matchedString.length)
        others <- tokenize(restString)
      } yield token +: others
    }
  }
}

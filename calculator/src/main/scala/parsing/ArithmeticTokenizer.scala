package parsing

import scala.util.matching.Regex

object ArithmeticTokenizer extends Tokenizer {

  lazy val regex: Regex = {
    ArithmeticTokenType.getInstances
      .map(tokenType => "(?<" + tokenType.toString + ">" + tokenType.regex.regex + ")")
      .mkString("|")
      .r
  }

  override def tokenize(rawString: String): Option[List[ArithmeticToken]] = {
    val string = rawString.trim
    if (string.isEmpty) {
      Some(List.empty)
    } else {
      val maybeMatchResult = regex.findPrefixMatchOf(string)
      val maybeMatchedString = maybeMatchResult.map(_.matched)
      val maybeTokenType = ArithmeticTokenType.getInstances
        .find(tokenType => maybeMatchResult.exists(_.group(tokenType.toString) != null))
      val maybeToken = ArithmeticToken(maybeTokenType, maybeMatchedString)
      for {
        token <- maybeToken
        matchedString <- maybeMatchedString
        restString = string.substring(matchedString.length)
        others <- tokenize(restString)
      } yield token +: others
    }
  }
}

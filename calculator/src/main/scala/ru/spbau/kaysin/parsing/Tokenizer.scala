package ru.spbau.kaysin.parsing

import scala.util.matching.Regex

/**
  * Represents entity which splits string into arithmetic tokens
  */
object Tokenizer {
  private val regex: Regex = {
    TokenType.getInstances
      .map(tokenType => "(?<" + tokenType.toString + ">" + tokenType.regex.regex + ")")
      .mkString("|")
      .r
  }

  /**
    * Splits given string into tokens.
    * @param rawString Input of the algorithm. The given string.
    * @return Some list of tokens if tokenizing completed successfully. Otherwise None.
    */
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

package ru.spbau.kaysin.hat.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * A parser which parses and categorizes user messages.
  */
class MessageParser extends RegexParsers {
  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  /**
    * Parse one word containing english or russian letters and digits
    */
  val wordParser: Parser[String] =
    """[\w[а-я][А-Я]]+""".r ^^ {
      _.toLowerCase
    }

  /**
    * Parse a bot command (starts with '/')
    */
  val commandParser: Parser[Command] =
    "/".r ~> wordParser <~ """.*""".r ^^ {
      case Start.command => Start
      case Play.command => Play
      case AddWords.command => AddWords
      case EndOfInput.command => EndOfInput
      case StartRound.command => StartRound
      case NextWord.command => NextWord
    }

  /**
    * Parse a word into UserWord wrapper.
    */
  val userWordParser: Parser[UserWord] =
    wordParser ^^ UserWord

  /**
    * General parser for all possible user messages
    */
  val userMessage: Parser[UserMessage] =
    commandParser | userWordParser
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}

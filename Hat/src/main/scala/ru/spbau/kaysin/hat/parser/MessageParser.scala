package ru.spbau.kaysin.hat.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override def skipWhitespace: Boolean = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  val wordParser: Parser[String] =
    """[\w[а-я][А-Я]]+""".r ^^ {
      _.toLowerCase
    }

  val commandParser: Parser[Command] =
    "/".r ~> wordParser <~ """.*""".r ^^ {
      case Start.command => Start
      case Play.command => Play
      case AddWords.command => AddWords
      case EndOfInput.command => EndOfInput
      case StartRound.command => StartRound
      case NextWord.command => NextWord
    }

  val userWordParser: Parser[UserWord] =
    wordParser ^^ UserWord

  val userMessage: Parser[UserMessage] =
    commandParser | userWordParser
}

object MessageParser extends MessageParser {
  def parser(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}

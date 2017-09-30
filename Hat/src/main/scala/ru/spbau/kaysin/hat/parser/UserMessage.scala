package ru.spbau.kaysin.hat.parser


sealed trait UserMessage

trait Command extends UserMessage {
  val command: String
}

case object Start extends Command {
  override val command = "start"
}
case object Play extends Command {
  override val command = "play"
}
case object AddWords extends Command {
  override val command = "add"
}
case object EndOfInput extends Command {
  override val command: String = "done"
}
case object StartRound extends Command {
  override val command = "run"
}
case object NextWord extends Command {
  override val command = "next"
}

case class UserWord(word: String) extends UserMessage

case object WrongMessage extends UserMessage


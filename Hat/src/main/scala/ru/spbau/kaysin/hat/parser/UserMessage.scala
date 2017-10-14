package ru.spbau.kaysin.hat.parser


sealed trait UserMessage

/**
  * User command. Something starting with '/'
  */
trait Command extends UserMessage {
  /**
    * Contained command.
    */
  val command: String
}

/**
  * Start the bot.
  */
case object Start extends Command {
  override val command = "start"
}

/**
  * Start a game.
  */
case object Play extends Command {
  override val command = "play"
}

/**
  * Add words into a hat.
  */
case object AddWords extends Command {
  override val command = "add"
}

/**
  * A message which user sends when he decides to end the list of words he added to the hat.
  */
case object EndOfInput extends Command {
  override val command: String = "done"
}

/**
  * Start a game round.
  */
case object StartRound extends Command {
  override val command = "run"
}

/**
  * Word is guessed. Ask for a new one.
  */
case object NextWord extends Command {
  override val command = "next"
}

/**
  * Ask for the rules of the game.
  */
case object Rules extends Command {
  override val command = "rules"
}

/**
  * A word sent by user. It might be a hat name or a new word to add.
  * @param word Wrapped word.
  */
case class UserWord(word: String) extends UserMessage

/**
  * An unrecognized message.
  */
case object WrongMessage extends UserMessage


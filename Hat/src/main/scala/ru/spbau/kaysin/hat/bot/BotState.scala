package ru.spbau.kaysin.hat.bot

import akka.actor.ActorRef
import ru.spbau.kaysin.hat.parser._


/**
  * A trait representing state of the bot. Each chat has it's own state.
  */
sealed trait BotState {
  /**
    * Message that should be printed when the state achieved
    */
  val message: String
}

/**
  * A state which expects commands from user.
  */
sealed trait WithCommands extends BotState {
  /**
    * List of commands with their descriptions
    */
  val commands: List[(Command, String)] // (command, description)

  /**
    * List of messages inside buttons shown to user.
    */
  lazy val buttons: List[String] = commands.map {case (cmd, dsc) => "/" ++ cmd.command ++ " - " ++ dsc}

  /**
    * If the buttons should be hidden after a user press them.
    */
  val oneTime: Boolean = true
}

case object MainMenu extends WithCommands {
  override val commands = List((AddWords, "Add words"), (Play, "Play"), (Rules, "Show the rules"))
  override val message = "You can add words to a hat or play it."
}

/**
  * A state when user should enter a hat name in which he wants to add new words.
  */
case object HatWaitingAdd extends BotState {
  override val message = "Which hat do you want to add words in? (Enter the hat name)"
}

/**
  * A state when user should enter a hat name he wants to play
  */
case object HatWaitingPlay extends BotState {
  override val message = "Which had do you want to play in? (Enter the hat name)"
}

/**
  * A state when user is adding new words into hat.
  * @param hat the hat to add new words.
  */
case class Adding(hat: String) extends WithCommands {
  override val commands = List((EndOfInput, "Done!"))
  override val message: String = s"Add new words one per message. When you are done with that, just write /${EndOfInput.command}."
}

/**
  * A state in one between game rounds.
  * It's supposed to pass the device to another player in this state.
  * @param game the game user plays.
  */
case class Interlude(game: ActorRef) extends WithCommands {
  override val commands = List((StartRound, "Start the round"))
  override val message: String = s"Enter /${StartRound.command} to start the round."
}

/**
  * A state corresponding to game round, when player gets words and tries to explain them.
  * @param game the game user play.
  */
case class Round(game: ActorRef) extends WithCommands{
  override val commands = List((NextWord, "Next word"))
  override val message: String = s"You have ${HatBotActor.roundTime} seconds. Let's go!"
  override val oneTime = false
}

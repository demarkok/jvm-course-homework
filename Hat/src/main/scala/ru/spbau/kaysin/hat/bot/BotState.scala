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
  override val commands = List((AddWords, "Добавить слова"), (Play, "Играть"))
  override val message = "Добавить слова в шляпу или играть?"
}

/**
  * A state when user should enter a hat name in which he wants to add new words.
  */
case object HatWaitingAdd extends BotState {
  override val message = "В какую шляпу?"
}

/**
  * A state when user should enter a hat name he wants to play
  */
case object HatWaitingPlay extends BotState {
  override val message = "В какую шляпу?"
}

/**
  * A state when user is adding new words into hat.
  * @param hat the hat to add new words.
  */
case class Adding(hat: String) extends WithCommands {
  override val commands = List((EndOfInput, "Закончить ввод"))
  override val message: String = "Добавляйте слова по одному. Когда закончите, пишите " ++ EndOfInput.command
}

/**
  * A state in one between game rounds.
  * It's supposed to pass the device to another player in this state.
  * @param game the game user plays.
  */
case class Interlude(game: ActorRef) extends WithCommands {
  override val commands = List((StartRound, "Начать раунд"))
  override val message: String = "Введите /" ++ StartRound.command ++ ", чтобы начать раунд."
}

/**
  * A state corresponding to game round, when player gets words and tries to explain them.
  * @param game the game user play.
  */
case class Round(game: ActorRef) extends WithCommands{
  override val commands = List((NextWord, "Следующее слово"))
  override val message: String = "У вас есть " ++ HatBotActor.roundTime.toString ++ "секунд!"
  override val oneTime = false
}

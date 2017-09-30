package ru.spbau.kaysin.hat.bot

import akka.actor.ActorRef
import ru.spbau.kaysin.hat.parser._

sealed trait WithCommands {
  val commands: List[(Command, String)] // (command, description)
  lazy val buttons: List[String] = commands.map {case (cmd, dsc) => "/" ++ cmd.command ++ " - " ++ dsc}
  val oneTime: Boolean = true
}

sealed trait BotState {
  val message: String
}

case object MainMenu extends BotState with WithCommands {
  override val commands = List((AddWords, "Добавить слова"), (Play, "Играть"))
  override val message = "Добавить слова в шляпу или играть?"
}
case object HatWaitingAdd extends BotState {
  override val message = "В какую шляпу?"
}
case object HatWaitingPlay extends BotState {
  override val message = "В какую шляпу?"
}
case class Adding(hat: String) extends BotState with WithCommands {
  override val commands = List((EndOfInput, "Закончить ввод"))
  override val message: String = "Добавляйте слова по одному. Когда закончите, пишите " ++ EndOfInput.command
}
case class Interlude(game: ActorRef) extends BotState with WithCommands {
  override val commands = List((StartRound, "Начать раунд"))
  override val message: String = "Введите /" ++ StartRound.command ++ ", чтобы начать раунд."
}
case class Round(game: ActorRef) extends BotState with WithCommands{
  override val commands = List((NextWord, "Следующее слово"))
  override val message: String = "У вас есть " ++ HatBotActor.roundTime.toString ++ "секунд!"
  override val oneTime = false
}

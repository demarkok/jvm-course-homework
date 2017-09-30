package ru.spbau.kaysin.hat

import akka.actor.ActorSystem
import ru.spbau.kaysin.hat.bot.HatBotActor
import ru.spbau.kaysin.hat.bot.HatBotActor.Run
import ru.spbau.kaysin.hat.database.HatStorageActor

object Main extends App {
  val token = "426751579:AAER_3R4WTtLhkQdBZKOtpHO7UJ5b7vEAWc"

  val system = ActorSystem()
  val database = system.actorOf(HatStorageActor.props())

  private val bot = system.actorOf(HatBotActor.props(token, database))
  bot ! Run
}

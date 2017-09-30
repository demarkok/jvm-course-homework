package ru.spbau.kaysin.hat.game

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import ru.spbau.kaysin.hat.bot.HatBotActor.{HatIsEmpty, TimeIsRunningOut, TimeIsUp, Word}
import ru.spbau.kaysin.hat.database.HatStorageActor.GetWords
import ru.spbau.kaysin.hat.game.GameActor._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Random, Success}



class GameActor(hat: String, id: Long, database: ActorRef)
               (implicit executionContext: ExecutionContext) extends Actor{
  var running: Boolean = false
  var wordsGuessed: Int = 0
  var words: mutable.ListBuffer[String] = ListBuffer.empty

  implicit val timeout: Timeout = Timeout(1.second)

  (database ? GetWords(hat)).onComplete {
    case Success(Words(newWords)) =>
      words ++= newWords
  }


  override def receive: Receive = {
    case Start(time) =>
      running = true
      wordsGuessed = 0

      val bot = sender

      for (i <- time - 5 until time) {
        context.system.scheduler.scheduleOnce(i seconds, bot, TimeIsRunningOut(id, time - i))
      }

      context.system.scheduler.scheduleOnce(time seconds) {
        if (running)  
          bot ! TimeIsUp(id, wordsGuessed)
        running = false
      }
      words = Random.shuffle(words)
    case GetWord =>
      if (words.isEmpty) {
        sender ! HatIsEmpty
        running = false
      } else {
        sender ! Word(words.head)
      }
    case Guessed =>
      if (words.nonEmpty) {
        words.remove(0)
        wordsGuessed += 1
      }
  }
}

object GameActor {

  def props(hat: String, database: ActorRef)
           (implicit id: Long, executionContext: ExecutionContext): Props = Props(new GameActor(hat, id, database))

  case class Words(words: List[String])
  case class Start(timeInSecs: Int)
  case object GetWord
  case object Guessed

}

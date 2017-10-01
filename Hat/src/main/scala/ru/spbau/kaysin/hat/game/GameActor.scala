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


/**
  * An actor representing a game session.
  * @param hat The name of the hat in which user plays.
  * @param id Id of the chat where the game were started.
  * @param database Database actor.
  */
class GameActor(hat: String, id: Long, database: ActorRef)
               (implicit executionContext: ExecutionContext) extends Actor{
  private var running: Boolean = false
  private var wordsGuessed: Int = 0
  private var words: mutable.ListBuffer[String] = ListBuffer.empty

  implicit val timeout: Timeout = Timeout(60 seconds)

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
  /**
    * Function allowing to create the actor in a safe way.
    * @param hat passed to GameActor constructor
    * @param database passed to GameActor constructor
    * @param id passed to GameActor constructor
    * @param executionContext passed to GameActor constructor
    * @return Props object which is passed to system.actorOf
    */
  def props(hat: String, database: ActorRef)
           (implicit id: Long, executionContext: ExecutionContext): Props = Props(new GameActor(hat, id, database))

  /**
    * A response from database.
    * @param words The list of words contained in the hat.
    */
  case class Words(words: List[String])

  /**
    * A command to start the round.
    * @param timeInSecs -- Round duration in seconds.
    */
  case class Start(timeInSecs: Int)

  /**
    * Query to get a new random word from the hat.
    */
  case object GetWord

  /**
    * Responce which is send by user when whe word is guessed.
    */
  case object Guessed

}
